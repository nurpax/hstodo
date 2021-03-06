angular.module('components', ['todoServices']).
    directive('selecttags', function() {
        return {
            restrict: 'E',
            transclude: true,
            scope: {},
            controller: function($scope, $element, AppState) {
                $scope.tagList = function() { 
                    return AppState.getTags(); 
                };

                $scope.toggleTagFilter = function (tag) {
                    tag.enabled = !tag.enabled;
                };
            },
            template:
                '<div class="selecttags">'+
                '  <h2>Tags</h2>'+
                '    <ul class="unstyled">'+
                '      <li ng-click="toggleTagFilter(tag)" ng-repeat="tag in tagList()">'+
                '      <span class="label" ng-class="{selected: tag.enabled}">{{tag.tag}}</span>'+
                '    </li>'+
                '  </ul>'+
                '</div>',
            replace: true
        };
    })
    .directive('markdown', function() {
        var showdown = new Showdown.converter();

        return {
            restrict: 'E',
            scope: { content: '=' },
            link: function(scope, element, attrs) {
                // TODO kludgy, there must be a way without $watch & if (r)
                scope.$watch('content', function (r) {
                    if (r)
                        element.html(showdown.makeHtml(r));
                });
            }
        }
    })
