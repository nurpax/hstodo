
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
