
function NoteListCtrl($scope, Note, Tag) {
    // TODO query should probably only list notes with titles but not
    // take note body too
    $scope.notes = Note.query();
}

function NewNoteCtrl($scope, $location, $routeParams, Note, Tag) {
    $scope.note = {};

    $scope.save = function() {
        var n = { title:$scope.note.title, text:$scope.note.text };
        Note.save(n, function(note) {
            $location.path('/notes/view/' + note.id);
        });
    };
}

function ViewNoteCtrl($scope, $location, $routeParams, Note, Tag, AppState) {
    $scope.note = Note.get({id:$routeParams.noteId});
}


function EditNoteCtrl($scope, $location, $routeParams, Note, Tag, AppState) {
    $scope.note = Note.get({id:$routeParams.noteId});

    $scope.save = function() {
        Note.save($scope.note, function () {
            $location.path('/notes/view/' + $scope.note.id);
        });
    };

    $scope.cancel = function() {
        $location.path('/notes/view/' + $scope.note.id);
    };

}
