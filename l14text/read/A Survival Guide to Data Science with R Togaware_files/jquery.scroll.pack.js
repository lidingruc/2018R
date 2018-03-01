(function ($) {
    $(document).ready(function () {

        $(window).scroll(function () {
            if ($(this).scrollTop() > 100) {
                $('#backtotop').fadeIn();
                $('#backtotop').stop(true, true).fadeIn(200);
            } else {
                $('#backtotop').fadeOut();
                $('#backtotop').stop(true, true).fadeOut(200);
            }
        });

        $('#backtotop').click(function () {
            $("html, body").animate({scrollTop: 0}, 600);
            $('#backtotop').stop().animate({scrollTop: 0}, 600);
            return false;
        });

    });
})(jQuery);