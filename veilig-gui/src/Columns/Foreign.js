"use strict";

var $ = require("jquery");

exports.toggleColumns = function () {
    $('#right-column').toggleClass('col-lg-4 col-lg-push-8');
    $('#left-column').toggleClass('col-lg-8 col-lg-pull-4 col-lg-12');
}
