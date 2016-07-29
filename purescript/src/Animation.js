"use strict";

exports.requestAnimationFrame = function(callback) {
    return function() {
        return requestAnimationFrame(function(t) {
            callback(t)();
        });
    };
}

exports.cancelAnimationFrame = function(id) {
    return function() {
        cancelAnimationFrame(id);
    };
}
