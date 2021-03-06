
var frisby = require('frisby');

/* Upon entry to this test, the database schema of the target web app
 * is reset to empty.  */

function addTodo(text)
{
    frisby.create('Create todo')
        .post('http://localhost:8000/api/todo',
              {},
              { json: {
                  text: text,
                  done: false
              }
              })
        .expectStatus(200)
        .expectHeaderContains('content-type', 'application/json')
        .expectJSON({
            text: text
        })
        .toss();
}

function addTodoTag(todoId, tag)
{
    frisby.create('Add todo tag')
        .post('http://localhost:8000/api/todo/tag',
              {},
              { json: {
                  objectId: todoId,
                  tag: tag
              }
              })
        .expectStatus(200)
        .expectHeaderContains('content-type', 'application/json')
        .toss();
}

function findById(lst, id)
{
    var found = null;
    for (var i = 0; i < lst.length; i++) {
        if (lst[i].id == id)
            found = lst[i];
    }
    return found;
}

function activatesOnTests()
{
    frisby.create('Create todo with activatesOn:null')
        .post('http://localhost:8000/api/todo',
              {},
              { json: {
                  text: "todo act on 1",
                  done: false,
                  activatesOn: null
              }
              })
        .expectStatus(200)
        .expectHeaderContains('content-type', 'application/json')
        .expectJSON({
            text: "todo act on 1",
            activatesOn: null
        })
        .toss();

    frisby.create('Create todo with activatesOn:date')
        .post('http://localhost:8000/api/todo',
              {},
              { json: {
                  text: "todo act on 2",
                  done: false,
                  activatesOn: new Date('2000-01-01')
              }
              })
        .expectStatus(200)
        .expectHeaderContains('content-type', 'application/json')
        .expectJSON({
            text: "todo act on 2",
            activatesOn: '2000-01-01T00:00:00Z'
        })
        .afterJSON(function (todo) {
            // Spawn a new test on created todo response.  Test that
            // the created todo is still in the response
            frisby.create('List activates on todos')
                .get('http://localhost:8000/api/todo')
                .expectStatus(200)
                .expectHeaderContains('content-type', 'application/json')
                .afterJSON(function (lst) {
                    var found = findById(lst, todo.id);
                    expect(found).not.toBe(null);
                })
                .toss();

        })
        .toss();

    var timestamp = new Date();
    frisby.create('Check JS Date for activatesOn comes back from the server')
        .post('http://localhost:8000/api/todo',
              {},
              { json: {
                  text: "todo roundtrip",
                  done: false,
                  activatesOn: timestamp
              }
              })
        .expectStatus(200)
        .expectHeaderContains('content-type', 'application/json')
        .expectJSON({
            text: "todo roundtrip",
        })
        .afterJSON(function (todo) {
            expect(new Date(todo.activatesOn)).toEqual(timestamp);
        })
        .toss();

    var timestamp = new Date();
    frisby.create('Check JS Date for activatesOn comes back from the server')
        .post('http://localhost:8000/api/todo',
              {},
              { json: {
                  text: "active",
                  done: false,
                  activatesOn: timestamp
              }
              })
        .expectJSON({
            text: "active",
        })
        .afterJSON(function (todo) {
            // Spawn a new test on created todo response.  Test that
            // the created todo is still in the response
            var ts = timestamp.toISOString();
            frisby.create('Should find todo on activatesDate being same as todo.activatesOn')
                .get('http://localhost:8000/api/todo/?activatesDate='+ts)
                .expectStatus(200)
                .afterJSON(function (lst) {
                    var found = findById(lst, todo.id);
                    expect(found).not.toBe(null);
                })
                .toss();
            expect(new Date(todo.activatesOn)).toEqual(timestamp);

            var ts = new Date(timestamp - 5).toISOString();
            frisby.create('Should NOT find todo as activatesDate is in the future')
                .get('http://localhost:8000/api/todo/?activatesDate='+ts)
                .expectStatus(200)
                .afterJSON(function (lst) {
                    var found = findById(lst, todo.id);
                    expect(found).toBe(null);
                })
                .toss();
            expect(new Date(todo.activatesOn)).toEqual(timestamp);
        })
        .toss();

}

addTodo("test todo 1");

frisby.create('List one todo')
    .get('http://localhost:8000/api/todo')
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
    .expectJSON('0', {
        id: 1,
        text: "test todo 1"
    })
  .toss();

addTodo("test todo 2");

frisby.create('List two todos')
    .get('http://localhost:8000/api/todo')
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
    .expectJSON([{id: 1, text: "test todo 1", tags:[]},
                 {id: 2, text: "test todo 2", tags:[]}])
  .toss();

addTodoTag(1, "tag1");
addTodoTag(2, "tag2");
addTodoTag(1, "tag2");

frisby.create('List two tagged todos')
    .get('http://localhost:8000/api/todo')
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
    .expectJSON([{id: 1, text: "test todo 1", tags:[{tag:"tag1"}, {tag:"tag2"}]},
                 {id: 2, text: "test todo 2", tags:[{tag:"tag2"}]}])
  .toss();


frisby.create('List tags - check we can find tag1 and tag2')
    .get('http://localhost:8000/api/tag',
          {},
          {})
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
    .afterJSON(function (tags) {
        var foundTag1 = null;
        var foundTag2 = null;
        for (var i = 0; i < tags.length; i++) {
            if (tags[i].tag == 'tag1')
                foundTag1 = tags[i].id;
            if (tags[i].tag == 'tag2')
                foundTag2 = tags[i].id;
        }
        expect(foundTag1).not.toBe(null);
        expect(foundTag2).not.toBe(null);
    })
    .toss();


activatesOnTests();
