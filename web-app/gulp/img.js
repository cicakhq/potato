/* Images related tasks */
var gulp       = require('gulp');
var plumber    = require("gulp-plumber");

gulp.task('img:package', function () {
    return gulp.src([ 'src/images/**/*' ])
           .pipe(plumber())
           .pipe(gulp.dest('../public/assets/img'))
});
