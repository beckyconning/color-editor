/* global exports */
"use strict";

// module Keyboard
exports.addEventListenerImpl = function(eventName, fn, element) {
    return function() {
        element.addEventListener(eventName, function(e) {
            e.preventDefault();
            fn(e)();
        });
    };
};
