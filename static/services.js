'use strict';

angular.module('todoServices', ['ngResource']).
    factory('Todo', function($resource) {
        // AngularJS doesn't appear to support multiple URLs for
        // resources, hence the double $resource trickery here
        var defr =
            $resource('/api/todo', {}, {
                query:  {method:'GET', isArray:true},
                save:   {method:'POST'}
            });
        var addTagRes =
            $resource('/api/todo/tag', {}, {
                addTag: {method:'POST'}
            });

        defr.addTag = addTagRes.addTag.bind(addTagRes);
        return defr;
    }).
    factory('Tag', function($resource) {
        return $resource('/api/tag', {}, {
            query:  {method:'GET', isArray:true},
        });
    });
