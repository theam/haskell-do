/* global exports */
"use strict";

// module Data.Foreign.Callback


exports.callback0 = function(eff) {
  return function(){ return eff(); };
};

exports.callback1 = function(fn) {
  return function(a) { return fn(a)(); };
};

exports.callback2 = function(fn) {
  return function(a,b) { return fn(a)(b)(); };
};

exports.callback3 = function(fn) {
  return function(a,b,c) { return fn(a)(b)(c)(); };
};

exports.callback4 = function(fn) {
  return function(a,b,c,d) { return fn(a)(b)(c)(d)(); };
};

exports.callback5 = function(fn) {
  return function(a,b,c,d,e) { return fn(a)(b)(c)(d)(e)(); };
};

exports.callback6 = function(fn) {
  return function(a,b,c,d,e,f) { return fn(a)(b)(c)(d)(e)(f)(); };
};

exports.callback7 = function(fn) {
  return function(a,b,c,d,e,f,g) { return fn(a)(b)(c)(d)(e)(f)(g)(); };
};

exports.callback8 = function(fn) {
  return function(a,b,c,d,e,f,g,h) { return fn(a)(b)(c)(d)(e)(f)(g)(h)(); };
};

exports.callback9 = function(fn) {
  return function(a,b,c,d,e,f,g,h,i) { return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(); };
};

exports.callback10 = function(fn) {
  return function(a,b,c,d,e,f,g,h,i,j) { return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(); };
};
