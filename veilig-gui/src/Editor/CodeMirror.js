"use strict";

function objectify(array){
    var object = {};
    array.forEach(function(element) {
        object[element[0]] = element[1];
    });
    return object;
}

exports.fromTextArea = function (textAreaId) {
    return function (configArray) {
        return function () {
            if (configArray.length > 0) {
                return CodeMirror.fromTextArea(document.getElementById(textAreaId), objectify(configArray));
            } else {
                return CodeMirror.fromTextArea(document.getElementById(textAreaId));
            }
        }
    }
}

exports._onChange = function (editor) {
    return function (callback) {
        return function () {
            editor.on('change', function(newStuff) {
                return callback(newStuff.getValue());
            });
        }
    }
}
