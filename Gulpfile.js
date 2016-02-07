var gulp = require('gulp');
var elm = require('gulp-elm');
var plumber = require('gulp-plumber');
var express = require('express');
var morgan = require('morgan');

var paths = {
  dest: './dist',
  elm: './Todo.elm',
};

gulp.task('elm-init', elm.init);
gulp.task('elm', ['elm-init'], function() {
  return gulp.src(paths.elm)
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest(paths.dest));
});

gulp.task('default', ['elm'], function() {
  var app = express();
  app.use(morgan('dev'));
  app.use(express.static('./dist'));
  app.listen(4000);

  gulp.watch(paths.elm, ['elm']);
});
