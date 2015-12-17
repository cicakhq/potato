/* HTML related tasks */
var gulp       = require('gulp');
var plumber    = require("gulp-plumber");

gulp.task('html:watch', function () {
  return gulp.watch('src/html/**/*', [ 'html:compile' ]);
});

gulp.task('html:compile', function () {
  gulp.
    src('src/html/*', { nodir: true }).
    pipe(plumber()).
    pipe(gulp.dest('../public/assets/html'));
});
