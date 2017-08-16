(function($) {
    $.fn.goTo = function() {
        $('html, body').animate({
            scrollTop: $(this).offset().top + 'px'
        }, 'slow');
        return this; // for chaining...
    }
})(jQuery);


$(function() {
    $('.listing a').removeClass('selected');
    $('.listing a[href$="{{ slug }}"]').addClass('selected');

    $('.listing a').each(function() {
        if ($(this).attr('href').match(/\/legal\//)) {
            $(this).data('x-href',$(this).attr('href')).attr('href','#'+$(this).attr('href'));
            if (window.location.hash == $(this).attr('href')) {
                $('.listing a').removeClass('selected').filter(this).addClass('selected');
            }
        }
    });
    $('.listing a').on('click',function() {
        $('.listing a').removeClass('selected').filter(this).addClass('selected');
        if ($(this).data('x-href')) {
            $("#document").load($(this).data('x-href')+" #document" );
            if(Modernizr.mq('only screen and (max-width: 768px)')){
                $("#document").goTo();
            }
        }
    });
});