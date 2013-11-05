$(function($) {
var smoothScrollTo = function(el, opt_speed) {
    var speed = opt_speed || 300;
    var $el = $(el);
    if ($el.length !== 0) {
        var position = $el.offset().top;
        $(/safari/i.test(navigator.userAgent) ? 'body' : 'html').animate({
            scrollTop: position
        }, speed, 'swing', function() {
            var id =  $el.attr('id');
            if (!id) {
                id = $el.find('h1,h2').attr('id');
            }
            if (id && window.history && window.history.pushState) {
                history.pushState({}, null, '#'+id);
            }
        });
    }
};
$(document).on('click', 'a[href^=#]', function(e) {
    e.preventDefault();
    var href = $(this).attr("href");
    href = href.replace(/\./g, '\\.');
    var target = $(href == "#" || href == "" ? 'html' : href);
    smoothScrollTo(target);
});
})(jQuery);
