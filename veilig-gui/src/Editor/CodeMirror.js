"use strict";

function objectify(array){
    var object = {};
    array.forEach(function(element) {
        object[element[0]] = element[1];
    });
    return object;
}

exports.fromTextAreaImpl = function (textAreaId) {
    return function (configArray) {
        return function () {
            if (configArray.length > 0) {
                CodeMirror.fromTextArea(document.getElementById(textAreaId), objectify(configArray));
            } else {
                CodeMirror.fromTextArea(document.getElementById(textAreaId));
            }
        }
    }
}
