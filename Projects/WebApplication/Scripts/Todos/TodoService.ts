module todos {
    'use strict';

    /**
* Services that persists and retrieves TODOs from localStorage.
*/
    export class TodoService //implements ITodoStorage
    {
        public static $inject = [
            '$http'
        ];

        constructor(
            private $http: ng.IHttpService
            ) {
        }

        STORAGE_ID = 'todos-angularjs-typescript';

        get(): TodoItem[] {
            return JSON.parse(localStorage.getItem(this.STORAGE_ID) || '[]');
        }

        put(todos: TodoItem[]) {
            localStorage.setItem(this.STORAGE_ID, JSON.stringify(todos));
        }

        add(todo: TodoItem) {
            return this.$http.post("http://localhost:49185/api/todos/", todo)
                .success((data, status)=> true)
                .error((data, status)=> false);

        }

    }
}