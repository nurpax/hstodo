
var frisby = require('frisby');

frisby.create('Get (empty) todo list')
    .post('http://localhost:8000/api/todo',
          {},
          { json: {
              text: "test todo 1",
              done: false
          }
    })
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
.toss();
