'use strict';

function TodoItemCtrl($scope, Todo, Tag, AppState) {
    $scope.addTag = function() {
        var tag = { objectId:$scope.todo.id, tag:$scope.tagText };

        $scope.todo    = Todo.addTag(tag);
        $scope.tagText = '';

        AppState.setTags(Tag.query());
    };
}

function TodoCtrl($scope, Todo, Tag, AppState) {
    AppState.setTags(Tag.query());

    $scope.todos = Todo.query();

    $scope.addTodo = function() {
        var newTodo = { text:$scope.todoText, done:false };
        Todo.save(newTodo, function (todo) {
            $scope.todos.push(todo);
        });
        $scope.todoText = '';
    };

    // Persist immediate as clicked on
    $scope.checkTodo = function (todo) {
        todo.$save();
    };

    // Persist immediate as clicked on
    $scope.setActivatesOn = function (todo) {
        todo.$save();
    };

    $scope.remaining = function() {
        var count = 0;
        angular.forEach($scope.todos, function(todo) {
            count += todo.done ? 0 : 1;
        });
        return count;
    };

    $scope.archive = function() {
        var oldTodos = $scope.todos;
        $scope.todos = [];
        angular.forEach(oldTodos, function(todo) {
            if (!todo.done)
                $scope.todos.push(todo);
        });
    };

    // Filter todos based on selected tags
    $scope.doFilterByTag = function(todo) {
        var enabledTags = 0;
        var foundTags   = 0;
        angular.forEach(AppState.getTags(), function (tag) {
            if (tag.enabled)
            {
                enabledTags++;
                angular.forEach(todo.tags, function (todoTag) {
                    if (todoTag.id == tag.id)
                        foundTags++;
                });
            }
        });
        return enabledTags == foundTags;
    };
}

function NavList($scope, $location) {
    $scope.navClass = function (page) {
        var currentRoute = $location.path().split('/')[1] || 'home';
        return page === currentRoute ? 'active' : '';
    };
}

angular.module('todoApp', ['todoServices', 'components', 'ui']).
    config(['$routeProvider', function($routeProvider) {
    $routeProvider.
        when('/todos', {
            templateUrl: '/todos-partial.html',
            controller: TodoCtrl
        }).
        when('/notes', {
            templateUrl: '/notes-partial.html',
            controller: NoteListCtrl
        }).
        when('/notes/new', {
            templateUrl: '/note-edit-partial.html',
            controller: NewNoteCtrl
        }).
        when('/notes/view/:noteId', {
            templateUrl: '/note-view-partial.html',
            controller: ViewNoteCtrl
        }).
        when('/notes/edit/:noteId', {
            templateUrl: '/note-edit-partial.html',
            controller: EditNoteCtrl
        }).
        otherwise({
            redirectTo: '/todos'
        });
    }]);
