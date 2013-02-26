'use strict';

angular.module('todoServices', ['ngResource']).
    factory('Todo', function($resource) {
        // AngularJS doesn't appear to support multiple URLs for
        // resources, hence the double $resource trickery here

        // TODO timestamp gets converted into "isodatehere" when it's
        // run through params conversion.  Another option would be to
        // just stick "new Date()" into activatesDate below, but that
        // converts the date into a string format the server can't
        // parse.  D'oh.
        var timestamp = (new Date()).toISOString();
        var defr =
            $resource('/api/todo', {}, {
                query:  {method:'GET',
                         params:{
                             includeDone:false,
                             activatesDate:timestamp
                         }, isArray:true},
                save:   {method:'POST'}
            });
        var addTagRes =
            $resource('/api/todo/tag', {}, {
                addTag: {method:'POST'}
            });

        defr.addTag = addTagRes.addTag.bind(addTagRes);
        return defr;
    }).
    factory('Note', function($resource) {
        var defr =
            $resource('/api/note', {}, {
                get:    {method:'GET'},
                query:  {method:'GET', isArray:true},
                save:   {method:'POST'}
            });
        var addTagRes =
            $resource('/api/note/tag', {}, {
                addTag: {method:'POST'}
            });

        defr.addTag = addTagRes.addTag.bind(addTagRes);
        return defr;
    }).
    factory('Tag', function($resource) {
        return $resource('/api/tag', {}, {
            query:  {method:'GET', isArray:true},
        });
    }).
    factory('AppState', function() {
        this.tagList = [];
        return {
            setTags : function(lst) {
                this.tagList = lst;
            },
            getTags : function() {
                return this.tagList;
            }
        }
    });
