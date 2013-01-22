'use strict';

function TodoItemCtrl($scope, Todo, Tag) {
    $scope.addTag = function() {
        var tag = { todoId:$scope.todo.id, tag:$scope.tagText };

        $scope.todo    = Todo.addTag(tag);
        $scope.tagText = '';
        // TODO is there some way to do stuff like below with binding?
        //$scope.$parent.tags = Tag.query();
    };
}

function TodoCtrl($scope, Todo, Tag) {
    $scope.tags = Tag.query();
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

    $scope.toggleTagFilter = function (tag) {
        tag.enabled = !tag.enabled;
    };

    // Filter todos based on selected tags
    $scope.doFilterByTag = function(todo) {
        var enabledTags = 0;
        var foundTags   = 0;
        angular.forEach($scope.tags, function (tag) {
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


angular.module('todoApp', ['todoServices', 'ui']).
    config(['$routeProvider', function($routeProvider) {
    }]);
