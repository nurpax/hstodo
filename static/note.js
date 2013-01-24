
function NoteListCtrl($scope, Note, Tag) {
    // TODO query should probably only list notes with titles but not
    // take note body too
    $scope.notes = Note.query();

}

function NewNoteCtrl($scope, $location, Note, Tag) {
    $scope.save = function() {
        var n = { title:$scope.noteTitle, text:$scope.noteText };
        Note.save(n, function(note) {
            $location.path('/notes/edit/' + note.id);
        });
    };
}

function EditNoteCtrl($scope, $location, $routeParams, Note, Tag) {
    $scope.note = Note.get({id:$routeParams.noteId});

    $scope.save = function() {
        Note.save($scope.note, function () {
            $location.path('/notes');
        });
    };
}
