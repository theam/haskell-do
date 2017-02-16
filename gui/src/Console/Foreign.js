"use strict";

const animationType = "flash"

exports.animateSaveButton = function () {
    $('#save-button').addClass('animated ' + animationType).one('webkitAnimationEnd mozAnimationEnd MSAnimationEnd oanimationend animationend',
        function(){
            $(this).removeClass('animated ' + animationType)
        }
    );
}