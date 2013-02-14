
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
                    var found = null;
                    for (var i = 0; i < lst.length; i++) {
                        if (lst[i].id == todo.id)
                            found = lst[i];
                    }
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

activatesOnTests();
