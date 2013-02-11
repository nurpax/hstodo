
var frisby = require('frisby');

/* Upon entry to this test, the database schema of the target web app
 * is reset to empty.  */

frisby.create('Create one todo')
    .post('http://localhost:8000/api/todo',
          {},
          { json: {
              text: "test todo 1",
              done: false
          }
    })
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
    .expectJSON({
        id: 1,
        text: "test todo 1"
    })
.toss();

frisby.create('List one todo')
    .get('http://localhost:8000/api/todo')
    .expectStatus(200)
    .expectHeaderContains('content-type', 'application/json')
    .expectJSON('0', {
        id: 1,
        text: "test todo 1"
    })
.toss();
