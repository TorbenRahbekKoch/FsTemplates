/// <reference path="../typings/angularjs/angular.d.ts" />
module todos {
    'use strict';

    var todoApp = angular.module('todoApp', [])
        .controller('todoController', TodoController)
        .service("todoService", TodoService);

    //todoApp.config([
    //    '$httpProvider', function($httpProvider) {
    //        $httpProvider.defaults.useXDomain = true;
    //        delete $httpProvider.defaults.headers.common['X-Requested-With'];
    //    }
    //]);
}

//// Handles accordion image changes
//$('.accordion').on('show hide', function (n) {
//    $(n.target).siblings('.accordion-toggle').find('div i').toggleClass('icon-chevron-up icon-chevron-down');
//});