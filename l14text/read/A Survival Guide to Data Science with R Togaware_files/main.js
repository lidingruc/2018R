//
// Responsive Primary Menu
//

if (js_responsive_menu.responsive_menu_layout == 'dropdown') {

    jQuery(document).ready(function () {
        if (js_responsive_menu.responsive_menu == '') {
            jQuery('.primary-menu .nav-holder .evolve_mobile_menu').meanmenu();
        } else {
            jQuery('.primary-menu .nav-holder .evolve_mobile_menu').meanmenu({
                meanMenuClose: "<label class='dd-selected-text'>" + js_responsive_menu.responsive_menu + "</label>",
                meanMenuOpen: "<label class='dd-selected-text'>" + js_responsive_menu.responsive_menu + "</label>"
            });
        }
    });

} else {

    // Create the dropdown base
    jQuery('<select />').appendTo('.primary-menu .nav-holder');

    // Create default option 'Menu'
    jQuery('<option />', {
        'selected': 'selected',
        'value': '',
        'text': js_responsive_menu.responsive_menu
    }).appendTo('.primary-menu .nav-holder select');

    // Populate dropdown with menu items
    jQuery('.primary-menu .nav-holder a').each(function () {
        var el = jQuery(this);

        if (jQuery(el).parents('.sub-menu .sub-menu').length >= 1) {
            jQuery('<option />', {
                'value': el.attr('href'),
                'text': '-- ' + el.text()
            }).appendTo('.primary-menu .nav-holder select');
        } else if (jQuery(el).parents('.sub-menu').length >= 1) {
            jQuery('<option />', {
                'value': el.attr('href'),
                'text': '- ' + el.text()
            }).appendTo('.primary-menu .nav-holder select');
        } else {
            jQuery('<option />', {
                'value': el.attr('href'),
                'text': el.text()
            }).appendTo('.primary-menu .nav-holder select');
        }
    });

    jQuery('.primary-menu .nav-holder select').ddslick({
        width: '100%',
        onSelected: function (selectedData) {
            if (selectedData.selectedData.value != '') {
                window.location = selectedData.selectedData.value;
            }
        }
    });
}

//
// Responsive Top Menu
//

// Create the dropdown base
jQuery('<select />').appendTo('.top-menu .nav-holder');

// Create default option 'Menu'
jQuery('<option />', {
    'selected': 'selected',
    'value': '',
    'text': js_responsive_menu.responsive_menu
}).appendTo('.top-menu .nav-holder select');

// Populate dropdown with menu items
jQuery('.top-menu .nav-holder a').each(function () {
    var el = jQuery(this);

    if (jQuery(el).parents('.sub-menu .sub-menu').length >= 1) {
        jQuery('<option />', {
            'value': el.attr('href'),
            'text': '-- ' + el.text()
        }).appendTo('.top-menu .nav-holder select');
    } else if (jQuery(el).parents('.sub-menu').length >= 1) {
        jQuery('<option />', {
            'value': el.attr('href'),
            'text': '- ' + el.text()
        }).appendTo('.top-menu .nav-holder select');
    } else {
        jQuery('<option />', {
            'value': el.attr('href'),
            'text': el.text()
        }).appendTo('.top-menu .nav-holder select');
    }
});

jQuery('.top-menu .nav-holder select').ddslick({
    width: '100%',
    onSelected: function (selectedData) {
        if (selectedData.selectedData.value != '') {
            window.location = selectedData.selectedData.value;
        }
    }
});


//
//
// 
// Responsive WooCommerce Menu
//
//
//   


// Create the dropdown base
jQuery('<select />').appendTo('.woocommerce-menu-holder .woocommerce-menu');

// Create default option 'Menu'
jQuery('<option />', {
    'selected': 'selected',
    'value': '',
    'text': '<span class="t4p-icon-shopping-cart"></span>'
}).appendTo('.woocommerce-menu-holder .woocommerce-menu select');

// Populate dropdown with menu items
jQuery('.woocommerce-menu-holder .woocommerce-menu a').each(function () {
    var el = jQuery(this);

    if (jQuery(el).parents('.sub-menu .sub-menu').length >= 1) {
        jQuery('<option />', {
            'value': el.attr('href'),
            'text': '-- ' + el.text()
        }).appendTo('.woocommerce-menu-holder .woocommerce-menu select');
    } else if (jQuery(el).parents('.sub-menu').length >= 1) {
        jQuery('<option />', {
            'value': el.attr('href'),
            'text': '- ' + el.text()
        }).appendTo('.woocommerce-menu-holder .woocommerce-menu select');
    } else {
        jQuery('<option />', {
            'value': el.attr('href'),
            'text': el.text()
        }).appendTo('.woocommerce-menu-holder .woocommerce-menu select');
    }
});

jQuery('.woocommerce-menu-holder .woocommerce-menu select').ddslick({
    width: '100%',
    onSelected: function (selectedData) {
        if (selectedData.selectedData.value != '') {
            window.location = selectedData.selectedData.value;
        }
    }
});


//
//
// 
// Tipsy
//
//
//


var $j = jQuery.noConflict();
$j(document).ready(function () {
    $j('.tipsytext').tipsy({gravity: 'n', fade: true, offset: 0, opacity: 1});
});

/* Video overlapping fix */

jQuery(document).ready(function () {
    jQuery('.t4p-slider iframe').each(function () {
        var url = jQuery(this).attr("src");
        if (url)
            if (url.indexOf("?") > 0) {
                jQuery(this).attr("src", url + "&wmode=transparent")
            } else {
                jQuery(this).attr("src", url + "?wmode=transparent")
            }
    });
});

/*
 * hoverFlow - A Solution to Animation Queue Buildup in jQuery
 * Version 1.00
 *
 * Copyright (c) 2009 Ralf Stoltze, http://www.2meter3.de/code/hoverFlow/
 * Dual-licensed under the MIT and GPL licenses.
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.gnu.org/licenses/gpl.html
 */
(function ($) {
    $.fn.hoverFlow = function (c, d, e, f, g) {
        if ($.inArray(c, ['mouseover', 'mouseenter', 'mouseout', 'mouseleave']) == -1) {
            return this
        }
        var h = typeof e === 'object' ? e : {complete: g || !g && f || $.isFunction(e) && e, duration: e, easing: g && f || f && !$.isFunction(f) && f};
        h.queue = false;
        var i = h.complete;
        h.complete = function () {
            $(this).dequeue();
            if ($.isFunction(i)) {
                i.call(this)
            }
        };
        return this.each(function () {
            var b = $(this);
            if (c == 'mouseover' || c == 'mouseenter') {
                b.data('jQuery.hoverFlow', true)
            } else {
                b.removeData('jQuery.hoverFlow')
            }
            b.queue(function () {
                var a = (c == 'mouseover' || c == 'mouseenter') ? b.data('jQuery.hoverFlow') !== undefined : b.data('jQuery.hoverFlow') === undefined;
                if (a) {
                    b.animate(d, h)
                } else {
                    b.queue([])
                }
            })
        })
    }
})(jQuery);

jQuery(window).load(function () {
    /*
     * Theme4Press Slider Settings
     */
    jQuery('#t4p-flexslider').flexslider({
        slideshow: js_local_vars.slide_auto_play,
        slideshowSpeed: js_local_vars.slide_show_speed,
        video: true,
        controlNav: js_local_vars.slide_pagination_circles,
        pauseOnHover: false,
        animation: js_local_vars.slide_animation,
        animationSpeed: js_local_vars.slide_animation_speed,
        directionNav: js_local_vars.slide_nav_arrows,
        animationLoop: true,
        useCSS: false

    });

});

jQuery(document).ready(function ($) {


    jQuery(".sb_toggle").click(function () {
        //Expand
        if (slidingbar_state == 0) {
            jQuery("div#slidingbar").slideDown(240, 'easeOutQuad');
            jQuery('.sb_toggle').addClass('open');
            slidingbar_state = 1;

            if (jQuery('#slidingbar .shortcode-map').length >= 1) {
                jQuery('#slidingbar').find('.shortcode-map').each(function () {
                    jQuery("#" + this.id).goMap();
                    marker = jQuery.goMap.markers[0];
                    if (marker) {
                        info = jQuery.goMap.getInfo(marker);
                        jQuery.goMap.setInfo(marker, info);
                    }
                    var center = jQuery.goMap.getMap().getCenter();
                    google.maps.event.trigger(jQuery.goMap.getMap(), 'resize');
                    jQuery.goMap.getMap().setCenter(center);
                });
            }
            //Collapse
        } else if (slidingbar_state == 1) {
            jQuery("div#slidingbar").slideUp(240, 'easeOutQuad');
            jQuery('.sb_toggle').removeClass('open');
            slidingbar_state = 0;
        }
    });

    /*! Copyright 2012, Ben Lin (http://dreamerslab.com/)
     * Licensed under the MIT License (LICENSE.txt).
     *
     * Version: 1.0.15
     *
     * Requires: jQuery >= 1.2.3
     */
    ;
    (function ($) {
        $.fn.addBack = $.fn.addBack || $.fn.andSelf;

        $.fn.extend({
            actual: function (method, options) {
                // check if the jQuery method exist
                if (!this[ method ]) {
                    throw '$.actual => The jQuery method "' + method + '" you called does not exist';
                }

                var defaults = {
                    absolute: false,
                    clone: false,
                    includeMargin: false
                };

                var configs = $.extend(defaults, options);

                var $target = this.eq(0);
                var fix, restore;

                if (configs.clone === true) {
                    fix = function () {
                        var style = 'position: absolute !important; top: -1000 !important; ';

                        // this is useful with css3pie
                        $target = $target.
                                clone().
                                attr('style', style).
                                appendTo('body');
                    };

                    restore = function () {
                        // remove DOM element after getting the width
                        $target.remove();
                    };
                } else {
                    var tmp = [];
                    var style = '';
                    var $hidden;

                    fix = function () {
                        // get all hidden parents
                        $hidden = $target.parents().addBack().filter(':hidden');
                        style += 'visibility: hidden !important; display: block !important; ';

                        if (configs.absolute === true)
                            style += 'position: absolute !important; ';

                        // save the origin style props
                        // set the hidden el css to be got the actual value later
                        $hidden.each(function () {
                            var $this = $(this);

                            // Save original style. If no style was set, attr() returns undefined
                            tmp.push($this.attr('style'));
                            $this.attr('style', style);
                        });
                    };

                    restore = function () {
                        // restore origin style values
                        $hidden.each(function (i) {
                            var $this = $(this);
                            var _tmp = tmp[ i ];

                            if (_tmp === undefined) {
                                $this.removeAttr('style');
                            } else {
                                $this.attr('style', _tmp);
                            }
                        });
                    };
                }

                fix();
                // get the actual value with user specific methed
                // it can be 'width', 'height', 'outerWidth', 'innerWidth'... etc
                // configs.includeMargin only works for 'outerWidth' and 'outerHeight'
                var actual = /(outer)/.test(method) ?
                        $target[ method ](configs.includeMargin) :
                        $target[ method ]();

                restore();
                // IMPORTANT, this plugin only return the value of the first element
                return actual;
            }
        });
    })(jQuery);

    // to top


    // Tabs
    //When page loads...
    jQuery('.tabs-wrapper').each(function () {
        jQuery(this).find(".tab_content").hide(); //Hide all content
        if (document.location.hash && jQuery(this).find("ul.tabs li a[href='" + document.location.hash + "']").length >= 1) {
            jQuery(this).find("ul.tabs li a[href='" + document.location.hash + "']").parent().addClass("active").show(); //Activate first tab
            jQuery(this).find(document.location.hash + ".tab_content").show(); //Show first tab content
        } else {
            jQuery(this).find("ul.tabs li:first").addClass("active").show(); //Activate first tab
            jQuery(this).find(".tab_content:first").show(); //Show first tab content
        }
    });

    jQuery('[data-toggle=modal]').live('click', function (e) {
        e.preventDefault();
    });

    //On Click Event
    jQuery("ul.tabs li").click(function (e) {
        jQuery(this).parents('.tabs-wrapper').find("ul.tabs li").removeClass("active"); //Remove any "active" class
        jQuery(this).addClass("active"); //Add "active" class to selected tab
        jQuery(this).parents('.tabs-wrapper').find(".tab_content").hide(); //Hide all tab content

        var activeTab = jQuery(this).find("a").attr("href"); //Find the href attribute value to identify the active tab + content
        jQuery(this).parents('.tabs-wrapper').find(activeTab).fadeIn(); //Fade in the active ID content

        jQuery(this).parents('.tabs-wrapper').find(activeTab).find('.content-boxes').each(function () {
            var cols = jQuery(this).find('.col').length;
            jQuery(this).addClass('columns-' + cols);
        });

        jQuery(this).parents('.tabs-wrapper').find(activeTab).find('.columns-3 .col:nth-child(3n), .columns-4 .col:nth-child(4n)').css('margin-right', '0px');

        jQuery(this).parents('.tabs-wrapper').find(activeTab).find('.portfolio-wrapper').isotope('reLayout');

        jQuery(this).parents('.tabs-wrapper').find(activeTab).find('.content-boxes-icon-boxed').each(function () {
            jQuery(this).find('.col').equalHeights();
        });

        jQuery(this).parents('.tabs-wrapper').find(activeTab).find('.shortcode-map').each(function () {
            jQuery("#" + this.id).goMap();
            marker = jQuery.goMap.markers[0];
            if (marker) {
                info = jQuery.goMap.getInfo(marker);
                jQuery.goMap.setInfo(marker, info);
            }
            var center = jQuery.goMap.getMap().getCenter();
            google.maps.event.trigger(jQuery.goMap.getMap(), 'resize');
            jQuery.goMap.getMap().setCenter(center);
        });

        generateCarousel();

        if (jQuery('.portfolio').length >= 1) {
            jQuery('.portfolio-wrapper').isotope('reLayout');
        }

        e.preventDefault();
    });

    jQuery("ul.tabs li a").click(function (e) {
        e.preventDefault();
    });

    jQuery('.woocommerce .images #carousel a').click(function (e) {
        e.preventDefault();
    });

    jQuery('.portfolio-tabs a').click(function (e) {
        e.preventDefault();

        var selector = jQuery(this).attr('data-filter');
        jQuery(this).parents('.portfolio').find('.portfolio-wrapper').isotope({filter: selector});

        jQuery(this).parents('ul').find('li').removeClass('active');
        jQuery(this).parent().addClass('active');
    });


    jQuery('.faq-tabs a').click(function (e) {
        e.preventDefault();

        var selector = jQuery(this).attr('data-filter');

        jQuery('.faqs .portfolio-wrapper .faq-item').fadeOut();
        jQuery('.faqs .portfolio-wrapper .faq-item' + selector).fadeIn();

        jQuery(this).parents('ul').find('li').removeClass('active');
        jQuery(this).parent().addClass('active');
    });

    jQuery('.toggle-content').each(function () {
        if (!jQuery(this).hasClass('default-open')) {
            jQuery(this).hide();
        }
    });

    jQuery("h5.toggle").click(function () {
        if (jQuery(this).parents('.accordian').length >= 1) {
            var accordian = jQuery(this).parents('.accordian');

            if (jQuery(this).hasClass('active')) {
                jQuery(accordian).find('h5.toggle').removeClass('active');
                jQuery(accordian).find(".toggle-content").slideUp();
            } else {
                jQuery(accordian).find('h5.toggle').removeClass('active');
                jQuery(accordian).find(".toggle-content").slideUp();

                jQuery(this).addClass('active');
                jQuery(this).next(".toggle-content").slideToggle();

                jQuery(this).parents('.accordian').find('.toggle-content .shortcode-map').each(function () {
                    jQuery("#" + this.id).goMap();
                    marker = jQuery.goMap.markers[0];
                    if (marker) {
                        info = jQuery.goMap.getInfo(marker);
                        jQuery.goMap.setInfo(marker, info);
                    }
                    var center = jQuery.goMap.getMap().getCenter();
                    google.maps.event.trigger(jQuery.goMap.getMap(), 'resize');
                    jQuery.goMap.getMap().setCenter(center);
                });

            }
        } else {
            if (jQuery(this).hasClass('active')) {
                jQuery(this).removeClass("active");
            } else {
                jQuery(this).addClass("active");
            }
        }

        generateCarousel();

        return false;
    });

    jQuery("h5.toggle").click(function () {
        if (!jQuery(this).parents('.accordian').length >= 1) {
            jQuery(this).next(".toggle-content").slideToggle();
        }
    });

    function isScrolledIntoView(elem)
    {
        var docViewTop = jQuery(window).scrollTop();
        var docViewBottom = docViewTop + jQuery(window).height();

        var elemTop = jQuery(elem).offset().top;
        var elemBottom = elemTop + jQuery(elem).height();

        return ((elemBottom <= docViewBottom) && (elemTop >= docViewTop));
    }

    jQuery('.toggle-alert').live('click', function (e) {
        e.preventDefault();

        jQuery(this).parent().slideUp();
    });


    jQuery('.columns-3 .col:nth-child(3n), .columns-4 .col:nth-child(4n)').css('margin-right', '0px');

    jQuery('input, textarea').placeholder();

    function checkForImage(url) {
        return(url.match(/\.(jpeg|jpg|gif|png)$/) != null);
    }


    generateCarousel();


    jQuery(window).load(function ($) {
        if (jQuery('.woocommerce-menu .cart').width() > 190) {
            jQuery('.woocommerce-menu .cart-contents').css("width", jQuery('.woocommerce-menu .cart').width());
            jQuery('.woocommerce-menu .cart-content a').css("width", jQuery('.woocommerce-menu .cart').width() - 26);
            jQuery('.woocommerce-menu .cart-content a .cart-desc').css("width", jQuery('.woocommerce-menu .cart').width() - 82);
        }
        ;

    });

    // Woocommerce

    jQuery('.catalog-ordering .orderby .current-li a').html(jQuery('.catalog-ordering .orderby ul li.current a').html());
    jQuery('.catalog-ordering .sort-count .current-li a').html(jQuery('.catalog-ordering .sort-count ul li.current a').html());
    jQuery('.woocommerce #calc_shipping_state').parent().addClass('one_half');
    jQuery('.woocommerce #calc_shipping_postcode').parent().addClass('one_half last');
    jQuery('.woocommerce .shop_table .variation dd').after('<br />');
    jQuery('.woocommerce .evolve-myaccount-data th.order-actions').text(js_local_vars.order_actions);

    jQuery('.rtl .woocommerce .wc-forward').each(function () {
        jQuery(this).val(jQuery('.rtl .woocommerce .wc-forward').val().replace('\u2192', '\u2190'));
    });

    jQuery('.woocommerce input').each(function () {
        if (!jQuery(this).has('#coupon_code')) {
            name = jQuery(this).attr('id');
            jQuery(this).attr('placeholder', jQuery(this).parent().find('label[for=' + name + ']').text());
        }
    });


    if (jQuery('.woocommerce #reviews #comments .comment_container .comment-text').length) {
        jQuery('.woocommerce #reviews #comments .comment_container').append('<div class="clear"></div>');
    }

    if (jQuery('.woocommerce.single-product .related.products > h2').length) {
        jQuery('.woocommerce.single-product .related.products > h2').wrap('<div class="title"></div>');
        jQuery('.woocommerce.single-product .related.products > .title').append('<div class="title-sep-container"><div class="title-sep"></div></div>');
    }

    if (jQuery('.woocommerce.single-product .upsells.products > h2').length) {
        jQuery('.woocommerce.single-product .upsells.products > h2').wrap('<div class="title"></div>');
        jQuery('.woocommerce.single-product .upsells.products > .title').append('<div class="title-sep-container"><div class="title-sep"></div></div>');
    }

    if (jQuery('body #sidebar').css('display') == "block") {
        jQuery('body').addClass('has-sidebar');
        calcTabsLayout('.woocommerce-tabs .tabs-horizontal');
    }

    if (jQuery('body.archive.woocommerce #sidebar').css('display') == "block") {
        jQuery('#main ul.products').removeClass('products-1');
        jQuery('#main ul.products').removeClass('products-2');
        jQuery('#main ul.products').removeClass('products-4').addClass('products-3');
    }

    if (jQuery('body.single.woocommerce #sidebar').css('display') == "block") {
        jQuery('.upsells.products ul.products,.related.products ul.products').removeClass('products-1');
        jQuery('.upsells.products ul.products,.related.products ul.products').removeClass('products-2');
        jQuery('.upsells.products ul.products,.related.products ul.products').removeClass('products-4').addClass('products-3');
        jQuery('.upsells.products ul.products').html(jQuery('.upsells.products ul.products li').slice(0, 3));
        jQuery('.related.products ul.products').html(jQuery('.related.products ul.products li').slice(0, 3));
    }

    jQuery('#sidebar .products,.footer-area .products').each(function () {
        jQuery(this).removeClass('products-4');
        jQuery(this).removeClass('products-3');
        jQuery(this).removeClass('products-2');
        jQuery(this).addClass('products-1');
    });
    jQuery('.products-4 li, .products-3 li, .products-3 li').removeClass('last');

    $('.woocommerce-tabs ul.tabs li a').unbind('click');
    $('.woocommerce-tabs > ul.tabs li a').click(function () {

        var $tab = $(this);
        var $tabs_wrapper = $tab.closest('.woocommerce-tabs');

        $('ul.tabs li', $tabs_wrapper).removeClass('active');
        $('div.panel', $tabs_wrapper).hide();
        $('div' + $tab.attr('href'), $tabs_wrapper).show();
        $tab.parent().addClass('active');

        return false;
    });

    jQuery('.woocommerce-checkout-nav a,.continue-checkout').click(function (e) {
        e.preventDefault();

        var data_name = $(this).attr('data-name');
        var name = data_name;
        if (data_name != '#order_review') {
            name = '.' + data_name;
        }

        jQuery('form.checkout .col-1, form.checkout .col-2, form.checkout #order_review_heading, form.checkout #order_review').hide();
        jQuery('form.checkout').find(name).fadeIn();
        if (name == '#order_review') {
            jQuery('form.checkout').find('#order_review_heading').fadeIn();
        }

        jQuery('.woocommerce-checkout-nav li').removeClass('active');
        jQuery('.woocommerce-checkout-nav').find('[data-name=' + data_name + ']').parent().addClass('active');
    });

    jQuery('.evolve-myaccount-nav a').click(function (e) {
        e.preventDefault();

        jQuery('.evolve-myaccount-data .view_dashboard, .evolve-myaccount-data .digital-downloads, .evolve-myaccount-data .my_account_orders, .evolve-myaccount-data .edit_address_heading, .evolve-myaccount-data .myaccount_address, .evolve-myaccount-data .edit-account-heading, .evolve-myaccount-data .edit-account-form').hide();

        if (jQuery(this).hasClass('downloads')) {
            jQuery('.evolve-myaccount-data .digital-downloads').fadeIn();
        } else if (jQuery(this).hasClass('orders')) {
            jQuery('.evolve-myaccount-data .my_account_orders').fadeIn();
        } else if (jQuery(this).hasClass('address')) {
            jQuery('.evolve-myaccount-data .edit_address_heading, .evolve-myaccount-data .myaccount_address').fadeIn();
        } else if (jQuery(this).hasClass('account')) {
            jQuery('.evolve-myaccount-data .edit-account-heading, .evolve-myaccount-data .edit-account-form').fadeIn();
        } else if (jQuery(this).hasClass('dashboard')) {
            jQuery('.evolve-myaccount-data .view_dashboard').fadeIn();
        }

        jQuery('.evolve-myaccount-nav li').removeClass('active');
        jQuery(this).parent().addClass('active');
    });

    jQuery('a.add_to_cart_button').click(function (e) {
        var link = this;
        jQuery(link).closest('.product').find('.cart-loading').find('i').removeClass('t4p-icon-ok').addClass('t4p-icon-ok');
        jQuery(this).closest('.product').find('.cart-loading').fadeIn();
        setTimeout(function () {
            jQuery(link).closest('.product').find('.product-images img').animate({opacity: 0.75});
            jQuery(link).closest('.product').find('.cart-loading').find('i').hide().removeClass('t4p-icon-repeat').addClass('t4p-icon-ok').fadeIn();

            setTimeout(function () {
                jQuery(link).closest('.product').find('.cart-loading').fadeOut().closest('.product').find('.product-images img').animate({opacity: 1});
            }, 2000);
        }, 2000);
    });

    jQuery('li.product').mouseenter(function () {
        if (jQuery(this).find('.cart-loading').find('i').hasClass('t4p-icon-ok')) {
            jQuery(this).find('.cart-loading').fadeIn();
        }
    }).mouseleave(function () {
        if (jQuery(this).find('.cart-loading').find('i').hasClass('t4p-icon-ok')) {
            jQuery(this).find('.cart-loading').stop().fadeOut('400');
        }
    });


    jQuery('.sep-boxed-pricing,.full-boxed-pricing').each(function () {
        jQuery(this).addClass('columns-' + jQuery(this).find('.column').length);
    });

    jQuery('.content-boxes-icon-boxed').each(function () {
        jQuery(this).find('.col').equalHeights();
    });

    // wrap cf7 select and add arrow
    jQuery('.wpcf7-select:not([multiple])').wrap('<div class="wpcf7-select-parent"></div>');
    jQuery('<div class="select-arrow t4p-icon-angle-down"></div>').appendTo('.wpcf7-select-parent');

    // wrap cf7 select(with multiple) and add arrow
    jQuery('.wpcf7-select[multiple]').wrap('<div class="wpcf7-select-parent"></div>');

    // wrap variation forms select and add arrow
    jQuery('table.variations select').wrap('<div class="evolve-select-parent"></div>');
    jQuery('<div class="evolve-select-arrow t4p-icon-angle-down"></div>').appendTo('table.variations .evolve-select-parent');

    // wrap gravity forms select and add arrow
    jQuery('.gform_wrapper select:not([multiple])').wrap('<div class="gravity-select-parent"></div>');
    jQuery('<div class="select-arrow t4p-icon-angle-down"></div>').appendTo('.gravity-select-parent');

    // wrap woo select and add arrow
    jQuery('.woocommerce #calc_shipping_country, .woocommerce .country_select, #bbp_stick_topic_select, #bbp_topic_status_select, #bbp_forum_id, #bbp_destination_topic,.woocommerce select#calc_shipping_state, .woocommerce select.state_select').wrap('<div class="evolve-select-parent"></div>').after('<div class="select-arrow t4p-icon-angle-down"></div>');

    // sticky menu logo vertical alignment center
    var parentHeight = jQuery('header.sticky-header').actual('height');
    var childHeight = jQuery('#sticky-logo').actual('height');
    jQuery('#sticky-logo').css('margin-top', (parentHeight - childHeight) / 2);
});

jQuery(document).bind('gform_post_conditional_logic', function () {
    var select = jQuery('.gform_wrapper select');
    jQuery(select).each(function () {
        if (jQuery(this).hasClass('chzn-done') && jQuery(this).parent().hasClass('gravity-select-parent')) {
            jQuery('.gform_wrapper select.chzn-done').unwrap('<div class="gravity-select-parent"></div>');
            jQuery('.gform_wrapper select.chzn-done').parent().find('.select-arrow').remove();
        }
    });
});


/*! http://mths.be/placeholder v2.0.7 by @mathias */
;
(function (window, document, $) {

    var isInputSupported = 'placeholder' in document.createElement('input');
    var isTextareaSupported = 'placeholder' in document.createElement('textarea');
    var prototype = $.fn;
    var valHooks = $.valHooks;
    var propHooks = $.propHooks;
    var hooks;
    var placeholder;

    if (isInputSupported && isTextareaSupported) {

        placeholder = prototype.placeholder = function () {
            return this;
        };

        placeholder.input = placeholder.textarea = true;

    } else {

        placeholder = prototype.placeholder = function () {
            var $this = this;
            $this
                    .filter((isInputSupported ? 'textarea' : ':input') + '[placeholder]')
                    .not('.placeholder')
                    .bind({
                        'focus.placeholder': clearPlaceholder,
                        'blur.placeholder': setPlaceholder
                    })
                    .data('placeholder-enabled', true)
                    .trigger('blur.placeholder');
            return $this;
        };

        placeholder.input = isInputSupported;
        placeholder.textarea = isTextareaSupported;

        hooks = {
            'get': function (element) {
                var $element = $(element);

                var $passwordInput = $element.data('placeholder-password');
                if ($passwordInput) {
                    return $passwordInput[0].value;
                }

                return $element.data('placeholder-enabled') && $element.hasClass('placeholder') ? '' : element.value;
            },
            'set': function (element, value) {
                var $element = $(element);

                var $passwordInput = $element.data('placeholder-password');
                if ($passwordInput) {
                    return $passwordInput[0].value = value;
                }

                if (!$element.data('placeholder-enabled')) {
                    return element.value = value;
                }
                if (value == '') {
                    element.value = value;
                    // Issue #56: Setting the placeholder causes problems if the element continues to have focus.
                    if (element != safeActiveElement()) {
                        // We can't use `triggerHandler` here because of dummy text/password inputs :(
                        setPlaceholder.call(element);
                    }
                } else if ($element.hasClass('placeholder')) {
                    clearPlaceholder.call(element, true, value) || (element.value = value);
                } else {
                    element.value = value;
                }
                // `set` can not return `undefined`; see http://jsapi.info/jquery/1.7.1/val#L2363
                return $element;
            }
        };

        if (!isInputSupported) {
            valHooks.input = hooks;
            propHooks.value = hooks;
        }
        if (!isTextareaSupported) {
            valHooks.textarea = hooks;
            propHooks.value = hooks;
        }

        $(function () {
            // Look for forms
            $(document).delegate('form', 'submit.placeholder', function () {
                // Clear the placeholder values so they don't get submitted
                var $inputs = $('.placeholder', this).each(clearPlaceholder);
                setTimeout(function () {
                    $inputs.each(setPlaceholder);
                }, 10);
            });
        });

        // Clear placeholder values upon page reload
        $(window).bind('beforeunload.placeholder', function () {
            $('.placeholder').each(function () {
                this.value = '';
            });
        });

    }

    function args(elem) {
        // Return an object of element attributes
        var newAttrs = {};
        var rinlinejQuery = /^jQuery\d+$/;
        $.each(elem.attributes, function (i, attr) {
            if (attr.specified && !rinlinejQuery.test(attr.name)) {
                newAttrs[attr.name] = attr.value;
            }
        });
        return newAttrs;
    }

    function clearPlaceholder(event, value) {
        var input = this;
        var $input = $(input);
        if (input.value == $input.attr('placeholder') && $input.hasClass('placeholder')) {
            if ($input.data('placeholder-password')) {
                $input = $input.hide().next().show().attr('id', $input.removeAttr('id').data('placeholder-id'));
                // If `clearPlaceholder` was called from `$.valHooks.input.set`
                if (event === true) {
                    return $input[0].value = value;
                }
                $input.focus();
            } else {
                input.value = '';
                $input.removeClass('placeholder');
                input == safeActiveElement() && input.select();
            }
        }
    }

    function setPlaceholder() {
        var $replacement;
        var input = this;
        var $input = $(input);
        var id = this.id;
        if (input.value == '') {
            if (input.type == 'password') {
                if (!$input.data('placeholder-textinput')) {
                    try {
                        $replacement = $input.clone().attr({'type': 'text'});
                    } catch (e) {
                        $replacement = $('<input>').attr($.extend(args(this), {'type': 'text'}));
                    }
                    $replacement
                            .removeAttr('name')
                            .data({
                                'placeholder-password': $input,
                                'placeholder-id': id
                            })
                            .bind('focus.placeholder', clearPlaceholder);
                    $input
                            .data({
                                'placeholder-textinput': $replacement,
                                'placeholder-id': id
                            })
                            .before($replacement);
                }
                $input = $input.removeAttr('id').hide().prev().attr('id', id).show();
                // Note: `$input[0] != input` now!
            }
            $input.addClass('placeholder');
            $input[0].value = $input.attr('placeholder');
        } else {
            $input.removeClass('placeholder');
        }
    }

    function safeActiveElement() {
        // Avoid IE9 `document.activeElement` of death
        // https://github.com/mathiasbynens/jquery-placeholder/pull/99
        try {
            return document.activeElement;
        } catch (err) {
        }
    }

}(this, document, jQuery));

/* hoverIntent is similar to jQuery's built-in "hover" method except that
 * instead of firing the handlerIn function immediately, hoverIntent checks
 * to see if the user's mouse has slowed down (beneath the sensitivity
 * threshold) before firing the event. The handlerOut function is only
 * called after a matching handlerIn.
 *
 * // basic usage ... just like .hover()
 * .hoverIntent( handlerIn, handlerOut )
 * .hoverIntent( handlerInOut )
 *
 * // basic usage ... with event delegation!
 * .hoverIntent( handlerIn, handlerOut, selector )
 * .hoverIntent( handlerInOut, selector )
 *
 * // using a basic configuration object
 * .hoverIntent( config )
 *
 * @param  handlerIn   function OR configuration object
 * @param  handlerOut  function OR selector for delegation OR undefined
 * @param  selector    selector OR undefined
 * @author Brian Cherne <brian(at)cherne(dot)net>
 */
(function ($) {
    $.fn.hoverIntent = function (handlerIn, handlerOut, selector) {

        // default configuration values
        var cfg = {
            interval: 100,
            sensitivity: 7,
            timeout: 0
        };

        if (typeof handlerIn === "object") {
            cfg = $.extend(cfg, handlerIn);
        } else if ($.isFunction(handlerOut)) {
            cfg = $.extend(cfg, {over: handlerIn, out: handlerOut, selector: selector});
        } else {
            cfg = $.extend(cfg, {over: handlerIn, out: handlerIn, selector: handlerOut});
        }

        // instantiate variables
        // cX, cY = current X and Y position of mouse, updated by mousemove event
        // pX, pY = previous X and Y position of mouse, set by mouseover and polling interval
        var cX, cY, pX, pY;

        // A private function for getting mouse position
        var track = function (ev) {
            cX = ev.pageX;
            cY = ev.pageY;
        };

        // A private function for comparing current and previous mouse position
        var compare = function (ev, ob) {
            ob.hoverIntent_t = clearTimeout(ob.hoverIntent_t);
            // compare mouse positions to see if they've crossed the threshold
            if ((Math.abs(pX - cX) + Math.abs(pY - cY)) < cfg.sensitivity) {
                $(ob).off("mousemove.hoverIntent", track);
                // set hoverIntent state to true (so mouseOut can be called)
                ob.hoverIntent_s = 1;
                return cfg.over.apply(ob, [ev]);
            } else {
                // set previous coordinates for next time
                pX = cX;
                pY = cY;
                // use self-calling timeout, guarantees intervals are spaced out properly (avoids JavaScript timer bugs)
                ob.hoverIntent_t = setTimeout(function () {
                    compare(ev, ob);
                }, cfg.interval);
            }
        };

        // A private function for delaying the mouseOut function
        var delay = function (ev, ob) {
            ob.hoverIntent_t = clearTimeout(ob.hoverIntent_t);
            ob.hoverIntent_s = 0;
            return cfg.out.apply(ob, [ev]);
        };

        // A private function for handling mouse 'hovering'
        var handleHover = function (e) {
            // copy objects to be passed into t (required for event object to be passed in IE)
            var ev = jQuery.extend({}, e);
            var ob = this;

            // cancel hoverIntent timer if it exists
            if (ob.hoverIntent_t) {
                ob.hoverIntent_t = clearTimeout(ob.hoverIntent_t);
            }

            // if e.type == "mouseenter"
            if (e.type == "mouseenter") {
                // set "previous" X and Y position based on initial entry point
                pX = ev.pageX;
                pY = ev.pageY;
                // update "current" X and Y position based on mousemove
                $(ob).on("mousemove.hoverIntent", track);
                // start polling interval (self-calling timeout) to compare mouse coordinates over time
                if (ob.hoverIntent_s != 1) {
                    ob.hoverIntent_t = setTimeout(function () {
                        compare(ev, ob);
                    }, cfg.interval);
                }

                // else e.type == "mouseleave"
            } else {
                // unbind expensive mousemove event
                $(ob).off("mousemove.hoverIntent", track);
                // if hoverIntent state is true, then call the mouseOut function after the specified delay
                if (ob.hoverIntent_s == 1) {
                    ob.hoverIntent_t = setTimeout(function () {
                        delay(ev, ob);
                    }, cfg.timeout);
                }
            }
        };

        // listen for mouseenter and mouseleave
        return this.on({'mouseenter.hoverIntent': handleHover, 'mouseleave.hoverIntent': handleHover}, cfg.selector);
    };
})(jQuery);

/* UItoTop jQuery Plugin 1.2 | Matt Varone | http://www.mattvarone.com/web-design/uitotop-jquery-plugin */
(function ($) {
    $.fn.UItoTop = function (options) {
        var defaults = {text: '', min: 200, inDelay: 600, outDelay: 400, containerID: 'toTop', containerHoverID: 'toTopHover', scrollSpeed: 1200, easingType: 'linear'}, settings = $.extend(defaults, options), containerIDhash = '#' + settings.containerID, containerHoverIDHash = '#' + settings.containerHoverID;
        $('body').append('<a href="#" id="' + settings.containerID + '">' + settings.text + '</a>');
        $(containerIDhash).hide().on('click.UItoTop', function () {
            $('html, body').animate({scrollTop: 0}, settings.scrollSpeed, settings.easingType);
            $('#' + settings.containerHoverID, this).stop().animate({'opacity': 0}, settings.inDelay, settings.easingType);
            return false;
        }).prepend('<span id="' + settings.containerHoverID + '"></span>').hover(function () {
            $(containerHoverIDHash, this).stop().animate({'opacity': 1}, 600, 'linear');
        }, function () {
            $(containerHoverIDHash, this).stop().animate({'opacity': 0}, 700, 'linear');
        });
        $(window).scroll(function () {
            var sd = $(window).scrollTop();
            if (typeof document.body.style.maxHeight === "undefined") {
                $(containerIDhash).css({'position': 'absolute', 'top': sd + $(window).height() - 50});
            }
            if (sd > settings.min)
                $(containerIDhash).fadeIn(settings.inDelay);
            else
                $(containerIDhash).fadeOut(settings.Outdelay);
        });
    };
})(jQuery);

(function (window, $, undefined) {

    /*
     * smartresize: debounced resize event for jQuery
     *
     * latest version and complete README available on Github:
     * https://github.com/louisremi/jquery.smartresize.js
     *
     * Copyright 2011 @louis_remi
     * Licensed under the MIT license.
     */

    var $event = $.event, resizeTimeout;

    $event.special.smartresize = {
        setup: function () {
            $(this).bind("resize", $event.special.smartresize.handler);
        },
        teardown: function () {
            $(this).unbind("resize", $event.special.smartresize.handler);
        },
        handler: function (event, execAsap) {
            // Save the context
            var context = this,
                    args = arguments;

            // set correct event type
            event.type = "smartresize";

            if (resizeTimeout) {
                clearTimeout(resizeTimeout);
            }
            resizeTimeout = setTimeout(function () {
                jQuery.event.handle.apply(context, args);
            }, execAsap === "execAsap" ? 0 : 100);
        }
    };

    $.fn.smartresize = function (fn) {
        return fn ? this.bind("smartresize", fn) : this.trigger("smartresize", ["execAsap"]);
    };

    $.Slideshow = function (options, element) {

        this.$el = $(element);

        /***** images ****/

        // list of image items
        this.$list = this.$el.find('ul.ei-slider-large');
        // image items
        this.$imgItems = this.$list.children('li');
        // total number of items
        this.itemsCount = this.$imgItems.length;
        // images
        this.$images = this.$imgItems.find('img:first');

        /***** thumbs ****/

        // thumbs wrapper
        this.$sliderthumbs = this.$el.find('ul.ei-slider-thumbs').hide();
        // slider elements
        this.$sliderElems = this.$sliderthumbs.children('li');
        // sliding div
        this.$sliderElem = this.$sliderthumbs.children('li.ei-slider-element');
        // thumbs
        this.$thumbs = this.$sliderElems.not('.ei-slider-element');

        // initialize slideshow
        this._init(options);

    };

    $.Slideshow.defaults = {
        // animation types:
        // "sides" : new slides will slide in from left / right
        // "center": new slides will appear in the center
        animation: 'sides', // sides || center
        // if true the slider will automatically slide, and it will only stop if the user clicks on a thumb
        autoplay: false,
        // interval for the slideshow
        slideshow_interval: 3000,
        // speed for the sliding animation
        speed: 800,
        // easing for the sliding animation
        easing: '',
        // percentage of speed for the titles animation. Speed will be speed * titlesFactor
        titlesFactor: 0.60,
        // titles animation speed
        titlespeed: 800,
        // titles animation easing
        titleeasing: '',
        // maximum width for the thumbs in pixels
        thumbMaxWidth: 150
    };

    $.Slideshow.prototype = {
        _init: function (options) {

            this.options = $.extend(true, {}, $.Slideshow.defaults, options);

            // set the opacity of the title elements and the image items
            this.$imgItems.css('opacity', 0);
            this.$imgItems.find('div.ei-title > *').css('opacity', 0);

            // index of current visible slider
            this.current = 0;

            var _self = this;

            // preload images
            // add loading status
            this.$loading = $('<div class="ei-slider-loading">Loading</div>').prependTo(_self.$el);

            $.when(this._preloadImages()).done(function () {

                // hide loading status
                _self.$loading.hide();

                // calculate size and position for each image
                _self._setImagesSize();

                // configure thumbs container
                _self._initThumbs();

                // show first
                _self.$imgItems.eq(_self.current).css({
                    'opacity': 1,
                    'z-index': 10
                }).show().find('div.ei-title > *').css('opacity', 1);

                // if autoplay is true
                if (_self.options.autoplay) {

                    _self._startSlideshow();

                }

                // initialize the events
                _self._initEvents();

            });

        },
        _preloadImages: function () {

            // preloads all the large images

            var _self = this,
                    loaded = 0;

            return $.Deferred(
                    function (dfd) {

                        _self.$images.each(function (i) {

                            $('<img/>').load(function () {

                                if (++loaded === _self.itemsCount) {

                                    dfd.resolve();

                                }

                            }).attr('src', $(this).attr('src'));

                        });

                    }

            ).promise();

        },
        _setImagesSize: function () {

            // save ei-slider's width
            this.elWidth = this.$el.width();

            var _self = this;

            this.$images.each(function (i) {

                var $img = $(this);
                imgDim = _self._getImageDim($img.attr('src'));

                $img.css({
                    width: imgDim.width,
                    height: imgDim.height,
                    marginLeft: imgDim.left,
                    marginTop: imgDim.top
                });

            });

        },
        _getImageDim: function (src) {

            var $img = new Image();

            $img.src = src;

            var c_w = this.elWidth,
                    c_h = this.$el.height(),
                    r_w = c_h / c_w,
                    i_w = $img.width,
                    i_h = $img.height,
                    r_i = i_h / i_w,
                    new_w, new_h, new_left, new_top;

            if (r_w > r_i) {

                new_h = c_h;
                new_w = c_h / r_i;

            } else {

                new_h = c_w * r_i;
                new_w = c_w;

            }

            return {
                width: new_w,
                height: new_h,
                left: (c_w - new_w) / 2,
                top: (c_h - new_h) / 2
            };

        },
        _initThumbs: function () {

            // set the max-width of the slider elements to the one set in the plugin's options
            // also, the width of each slider element will be 100% / total number of elements
            this.$sliderElems.css({
                'max-width': this.options.thumbMaxWidth + 'px',
                'width': 100 / this.itemsCount + '%'
            });

            // set the max-width of the slider and show it
            this.$sliderthumbs.css('max-width', this.options.thumbMaxWidth * this.itemsCount + 'px').show();

        },
        _startSlideshow: function () {

            var _self = this;

            this.slideshow = setTimeout(function () {

                var pos;

                (_self.current === _self.itemsCount - 1) ? pos = 0 : pos = _self.current + 1;

                _self._slideTo(pos);

                if (_self.options.autoplay) {

                    _self._startSlideshow();

                }

            }, this.options.slideshow_interval);

        },
        // shows the clicked thumb's slide
        _slideTo: function (pos) {

            // return if clicking the same element or if currently animating
            if (pos === this.current || this.isAnimating)
                return false;

            this.isAnimating = true;

            var $currentSlide = this.$imgItems.eq(this.current),
                    $nextSlide = this.$imgItems.eq(pos),
                    _self = this,
                    preCSS = {zIndex: 10},
            animCSS = {opacity: 1};

            // new slide will slide in from left or right side
            if (this.options.animation === 'sides') {

                preCSS.left = (pos > this.current) ? -1 * this.elWidth : this.elWidth;
                animCSS.left = 0;

            }

            // titles animation
            $nextSlide.find('div.ei-title > h2')
                    .css('margin-right', 50 + 'px')
                    .stop()
                    .delay(this.options.speed * this.options.titlesFactor)
                    .animate({marginRight: 0 + 'px', opacity: 1}, this.options.titlespeed, this.options.titleeasing)
                    .end()
                    .find('div.ei-title > h3')
                    .css('margin-right', -50 + 'px')
                    .stop()
                    .delay(this.options.speed * this.options.titlesFactor)
                    .animate({marginRight: 0 + 'px', opacity: 1}, this.options.titlespeed, this.options.titleeasing);

            $.when(
                    // fade out current titles
                    $currentSlide.css('z-index', 1).find('div.ei-title > *').stop().fadeOut(this.options.speed / 2, function () {
                // reset style
                $(this).show().css('opacity', 0);
            }),
                    // animate next slide in
                    $nextSlide.css(preCSS).stop().animate(animCSS, this.options.speed, this.options.easing),
                    // "sliding div" moves to new position
                    this.$sliderElem.stop().animate({
                left: this.$thumbs.eq(pos).position().left
            }, this.options.speed)

                    ).done(function () {

                // reset values
                $currentSlide.css('opacity', 0).find('div.ei-title > *').css('opacity', 0);
                _self.current = pos;
                _self.isAnimating = false;

            });

        },
        _initEvents: function () {

            var _self = this;

            // window resize
            $(window).on('smartresize.eislideshow', function (event) {

                // resize the images
                _self._setImagesSize();

                // reset position of thumbs sliding div
                _self.$sliderElem.css('left', _self.$thumbs.eq(_self.current).position().left);

            });

            // click the thumbs
            this.$thumbs.on('click.eislideshow', function (event) {

                if (_self.options.autoplay) {

                    clearTimeout(_self.slideshow);
                    _self.options.autoplay = false;

                }

                var $thumb = $(this),
                        idx = $thumb.index() - 1; // exclude sliding div

                _self._slideTo(idx);

                return false;

            });

        }
    };

    var logError = function (message) {

        if (this.console) {

            console.error(message);

        }

    };

    $.fn.eislideshow = function (options) {

        if (typeof options === 'string') {

            var args = Array.prototype.slice.call(arguments, 1);

            this.each(function () {

                var instance = $.data(this, 'eislideshow');

                if (!instance) {
                    logError("cannot call methods on eislideshow prior to initialization; " +
                            "attempted to call method '" + options + "'");
                    return;
                }

                if (!$.isFunction(instance[options]) || options.charAt(0) === "_") {
                    logError("no such method '" + options + "' for eislideshow instance");
                    return;
                }

                instance[ options ].apply(instance, args);

            });

        } else {

            this.each(function () {

                var instance = $.data(this, 'eislideshow');
                if (!instance) {
                    $.data(this, 'eislideshow', new $.Slideshow(options, this));
                }

            });

        }

        return this;

    };

})(window, jQuery);

/*!
 * jQuery Cycle Lite Plugin
 * http://malsup.com/jquery/cycle/lite/
 * Copyright (c) 2008-2012 M. Alsup
 * Version: 1.7 (20-FEB-2013)
 * Dual licensed under the MIT and GPL licenses:
 * http://www.opensource.org/licenses/mit-license.php
 * http://www.gnu.org/licenses/gpl.html
 * Requires: jQuery v1.3.2 or later
 */
;
(function ($) {
    "use strict";

    var ver = 'Lite-1.7';
    var msie = /MSIE/.test(navigator.userAgent);

    $.fn.cycle = function (options) {
        return this.each(function () {
            options = options || {};

            if (this.cycleTimeout)
                clearTimeout(this.cycleTimeout);

            this.cycleTimeout = 0;
            this.cyclePause = 0;

            var $cont = $(this);
            var $slides = options.slideExpr ? $(options.slideExpr, this) : $cont.children();
            var els = $slides.get();
            if (els.length < 2) {
                if (window.console)
                    console.log('terminating; too few slides: ' + els.length);
                return; // don't bother
            }

            // support metadata plugin (v1.0 and v2.0)
            var opts = $.extend({}, $.fn.cycle.defaults, options || {}, $.metadata ? $cont.metadata() : $.meta ? $cont.data() : {});
            var meta = $.isFunction($cont.data) ? $cont.data(opts.metaAttr) : null;
            if (meta)
                opts = $.extend(opts, meta);

            opts.before = opts.before ? [opts.before] : [];
            opts.after = opts.after ? [opts.after] : [];
            opts.after.unshift(function () {
                opts.busy = 0;
            });

            // allow shorthand overrides of width, height and timeout
            var cls = this.className;
            opts.width = parseInt((cls.match(/w:(\d+)/) || [])[1], 10) || opts.width;
            opts.height = parseInt((cls.match(/h:(\d+)/) || [])[1], 10) || opts.height;
            opts.timeout = parseInt((cls.match(/t:(\d+)/) || [])[1], 10) || opts.timeout;

            if ($cont.css('position') == 'static')
                $cont.css('position', 'relative');
            if (opts.width)
                $cont.width(opts.width);
            if (opts.height && opts.height != 'auto')
                $cont.height(opts.height);

            var first = 0;
            $slides.css({position: 'absolute', top: 0}).each(function (i) {
                $(this).css('z-index', els.length - i);
            });

            $(els[first]).css('opacity', 1).show(); // opacity bit needed to handle reinit case
            if (msie)
                els[first].style.removeAttribute('filter');

            if (opts.fit && opts.width)
                $slides.width(opts.width);
            if (opts.fit && opts.height && opts.height != 'auto')
                $slides.height(opts.height);
            if (opts.pause)
                $cont.hover(function () {
                    this.cyclePause = 1;
                }, function () {
                    this.cyclePause = 0;
                });

            var txFn = $.fn.cycle.transitions[opts.fx];
            if (txFn)
                txFn($cont, $slides, opts);

            $slides.each(function () {
                var $el = $(this);
                this.cycleH = (opts.fit && opts.height) ? opts.height : $el.height();
                this.cycleW = (opts.fit && opts.width) ? opts.width : $el.width();
            });

            if (opts.cssFirst)
                $($slides[first]).css(opts.cssFirst);

            if (opts.timeout) {
                // ensure that timeout and speed settings are sane
                if (opts.speed.constructor == String)
                    opts.speed = {slow: 600, fast: 200}[opts.speed] || 400;
                if (!opts.sync)
                    opts.speed = opts.speed / 2;
                while ((opts.timeout - opts.speed) < 250)
                    opts.timeout += opts.speed;
            }
            opts.speedIn = opts.speed;
            opts.speedOut = opts.speed;

            opts.slideCount = els.length;
            opts.currSlide = first;
            opts.nextSlide = 1;

            // fire artificial events
            var e0 = $slides[first];
            if (opts.before.length)
                opts.before[0].apply(e0, [e0, e0, opts, true]);
            if (opts.after.length > 1)
                opts.after[1].apply(e0, [e0, e0, opts, true]);

            if (opts.click && !opts.next)
                opts.next = opts.click;
            if (opts.next)
                $(opts.next).unbind('click.cycle').bind('click.cycle', function () {
                    return advance(els, opts, opts.rev ? -1 : 1);
                });
            if (opts.prev)
                $(opts.prev).unbind('click.cycle').bind('click.cycle', function () {
                    return advance(els, opts, opts.rev ? 1 : -1);
                });

            if (opts.timeout)
                this.cycleTimeout = setTimeout(function () {
                    go(els, opts, 0, !opts.rev);
                }, opts.timeout + (opts.delay || 0));
        });
    };

    function go(els, opts, manual, fwd) {
        if (opts.busy)
            return;
        var p = els[0].parentNode, curr = els[opts.currSlide], next = els[opts.nextSlide];
        if (p.cycleTimeout === 0 && !manual)
            return;

        if (manual || !p.cyclePause) {
            if (opts.before.length)
                $.each(opts.before, function (i, o) {
                    o.apply(next, [curr, next, opts, fwd]);
                });
            var after = function () {
                if (msie)
                    this.style.removeAttribute('filter');
                $.each(opts.after, function (i, o) {
                    o.apply(next, [curr, next, opts, fwd]);
                });
                queueNext(opts);
            };

            if (opts.nextSlide != opts.currSlide) {
                opts.busy = 1;
                $.fn.cycle.custom(curr, next, opts, after);
            }
            var roll = (opts.nextSlide + 1) == els.length;
            opts.nextSlide = roll ? 0 : opts.nextSlide + 1;
            opts.currSlide = roll ? els.length - 1 : opts.nextSlide - 1;
        } else {
            queueNext(opts);
        }

        function queueNext(opts) {
            if (opts.timeout)
                p.cycleTimeout = setTimeout(function () {
                    go(els, opts, 0, !opts.rev);
                }, opts.timeout);
        }
    }

// advance slide forward or back
    function advance(els, opts, val) {
        var p = els[0].parentNode, timeout = p.cycleTimeout;
        if (timeout) {
            clearTimeout(timeout);
            p.cycleTimeout = 0;
        }
        opts.nextSlide = opts.currSlide + val;
        if (opts.nextSlide < 0) {
            opts.nextSlide = els.length - 1;
        } else if (opts.nextSlide >= els.length) {
            opts.nextSlide = 0;
        }
        go(els, opts, 1, val >= 0);
        return false;
    }

    $.fn.cycle.custom = function (curr, next, opts, cb) {
        var $l = $(curr), $n = $(next);
        $n.css(opts.cssBefore);
        var fn = function () {
            $n.animate(opts.animIn, opts.speedIn, opts.easeIn, cb);
        };
        $l.animate(opts.animOut, opts.speedOut, opts.easeOut, function () {
            $l.css(opts.cssAfter);
            if (!opts.sync)
                fn();
        });
        if (opts.sync)
            fn();
    };

    $.fn.cycle.transitions = {
        fade: function ($cont, $slides, opts) {
            $slides.not(':eq(0)').hide();
            opts.cssBefore = {opacity: 0, display: 'block'};
            opts.cssAfter = {display: 'none'};
            opts.animOut = {opacity: 0};
            opts.animIn = {opacity: 1};
        },
        fadeout: function ($cont, $slides, opts) {
            opts.before.push(function (curr, next, opts, fwd) {
                $(curr).css('zIndex', opts.slideCount + (fwd === true ? 1 : 0));
                $(next).css('zIndex', opts.slideCount + (fwd === true ? 0 : 1));
            });
            $slides.not(':eq(0)').hide();
            opts.cssBefore = {opacity: 1, display: 'block', zIndex: 1};
            opts.cssAfter = {display: 'none', zIndex: 0};
            opts.animOut = {opacity: 0};
            opts.animIn = {opacity: 1};
        }
    };

    $.fn.cycle.ver = function () {
        return ver;
    };

// @see: http://malsup.com/jquery/cycle/lite/
    $.fn.cycle.defaults = {
        animIn: {},
        animOut: {},
        fx: 'fade',
        after: null,
        before: null,
        cssBefore: {},
        cssAfter: {},
        delay: 0,
        fit: 0,
        height: 'auto',
        metaAttr: 'cycle',
        next: null,
        pause: false,
        prev: null,
        speed: 1000,
        slideExpr: null,
        sync: true,
        timeout: 4000
    };

})(jQuery);

/*!
 * imagesLoaded PACKAGED v3.0.4
 * JavaScript is all like "You images are done yet or what?"
 * MIT License
 */

(function () {
    "use strict";
    function e() {
    }
    function t(e, t) {
        for (var n = e.length; n--; )
            if (e[n].listener === t)
                return n;
        return-1
    }
    function n(e) {
        return function () {
            return this[e].apply(this, arguments)
        }
    }
    var i = e.prototype;
    i.getListeners = function (e) {
        var t, n, i = this._getEvents();
        if ("object" == typeof e) {
            t = {};
            for (n in i)
                i.hasOwnProperty(n) && e.test(n) && (t[n] = i[n])
        } else
            t = i[e] || (i[e] = []);
        return t
    }, i.flattenListeners = function (e) {
        var t, n = [];
        for (t = 0; e.length > t; t += 1)
            n.push(e[t].listener);
        return n
    }, i.getListenersAsObject = function (e) {
        var t, n = this.getListeners(e);
        return n instanceof Array && (t = {}, t[e] = n), t || n
    }, i.addListener = function (e, n) {
        var i, r = this.getListenersAsObject(e), o = "object" == typeof n;
        for (i in r)
            r.hasOwnProperty(i) && -1 === t(r[i], n) && r[i].push(o ? n : {listener: n, once: !1});
        return this
    }, i.on = n("addListener"), i.addOnceListener = function (e, t) {
        return this.addListener(e, {listener: t, once: !0})
    }, i.once = n("addOnceListener"), i.defineEvent = function (e) {
        return this.getListeners(e), this
    }, i.defineEvents = function (e) {
        for (var t = 0; e.length > t; t += 1)
            this.defineEvent(e[t]);
        return this
    }, i.removeListener = function (e, n) {
        var i, r, o = this.getListenersAsObject(e);
        for (r in o)
            o.hasOwnProperty(r) && (i = t(o[r], n), -1 !== i && o[r].splice(i, 1));
        return this
    }, i.off = n("removeListener"), i.addListeners = function (e, t) {
        return this.manipulateListeners(!1, e, t)
    }, i.removeListeners = function (e, t) {
        return this.manipulateListeners(!0, e, t)
    }, i.manipulateListeners = function (e, t, n) {
        var i, r, o = e ? this.removeListener : this.addListener, s = e ? this.removeListeners : this.addListeners;
        if ("object" != typeof t || t instanceof RegExp)
            for (i = n.length; i--; )
                o.call(this, t, n[i]);
        else
            for (i in t)
                t.hasOwnProperty(i) && (r = t[i]) && ("function" == typeof r ? o.call(this, i, r) : s.call(this, i, r));
        return this
    }, i.removeEvent = function (e) {
        var t, n = typeof e, i = this._getEvents();
        if ("string" === n)
            delete i[e];
        else if ("object" === n)
            for (t in i)
                i.hasOwnProperty(t) && e.test(t) && delete i[t];
        else
            delete this._events;
        return this
    }, i.removeAllListeners = n("removeEvent"), i.emitEvent = function (e, t) {
        var n, i, r, o, s = this.getListenersAsObject(e);
        for (r in s)
            if (s.hasOwnProperty(r))
                for (i = s[r].length; i--; )
                    n = s[r][i], n.once === !0 && this.removeListener(e, n.listener), o = n.listener.apply(this, t || []), o === this._getOnceReturnValue() && this.removeListener(e, n.listener);
        return this
    }, i.trigger = n("emitEvent"), i.emit = function (e) {
        var t = Array.prototype.slice.call(arguments, 1);
        return this.emitEvent(e, t)
    }, i.setOnceReturnValue = function (e) {
        return this._onceReturnValue = e, this
    }, i._getOnceReturnValue = function () {
        return this.hasOwnProperty("_onceReturnValue") ? this._onceReturnValue : !0
    }, i._getEvents = function () {
        return this._events || (this._events = {})
    }, "function" == typeof define && define.amd ? define(function () {
        return e
    }) : "object" == typeof module && module.exports ? module.exports = e : this.EventEmitter = e
}).call(this), function (e) {
    "use strict";
    var t = document.documentElement, n = function () {
    };
    t.addEventListener ? n = function (e, t, n) {
        e.addEventListener(t, n, !1)
    } : t.attachEvent && (n = function (t, n, i) {
        t[n + i] = i.handleEvent ? function () {
            var t = e.event;
            t.target = t.target || t.srcElement, i.handleEvent.call(i, t)
        } : function () {
            var n = e.event;
            n.target = n.target || n.srcElement, i.call(t, n)
        }, t.attachEvent("on" + n, t[n + i])
    });
    var i = function () {
    };
    t.removeEventListener ? i = function (e, t, n) {
        e.removeEventListener(t, n, !1)
    } : t.detachEvent && (i = function (e, t, n) {
        e.detachEvent("on" + t, e[t + n]);
        try {
            delete e[t + n]
        } catch (i) {
            e[t + n] = void 0
        }
    });
    var r = {bind: n, unbind: i};
    "function" == typeof define && define.amd ? define(r) : e.eventie = r
}(this), function (e) {
    "use strict";
    function t(e, t) {
        for (var n in t)
            e[n] = t[n];
        return e
    }
    function n(e) {
        return"[object Array]" === c.call(e)
    }
    function i(e) {
        var t = [];
        if (n(e))
            t = e;
        else if ("number" == typeof e.length)
            for (var i = 0, r = e.length; r > i; i++)
                t.push(e[i]);
        else
            t.push(e);
        return t
    }
    function r(e, n) {
        function r(e, n, s) {
            if (!(this instanceof r))
                return new r(e, n);
            "string" == typeof e && (e = document.querySelectorAll(e)), this.elements = i(e), this.options = t({}, this.options), "function" == typeof n ? s = n : t(this.options, n), s && this.on("always", s), this.getImages(), o && (this.jqDeferred = new o.Deferred);
            var a = this;
            setTimeout(function () {
                a.check()
            })
        }
        function c(e) {
            this.img = e
        }
        r.prototype = new e, r.prototype.options = {}, r.prototype.getImages = function () {
            this.images = [];
            for (var e = 0, t = this.elements.length; t > e; e++) {
                var n = this.elements[e];
                "IMG" === n.nodeName && this.addImage(n);
                for (var i = n.querySelectorAll("img"), r = 0, o = i.length; o > r; r++) {
                    var s = i[r];
                    this.addImage(s)
                }
            }
        }, r.prototype.addImage = function (e) {
            var t = new c(e);
            this.images.push(t)
        }, r.prototype.check = function () {
            function e(e, r) {
                return t.options.debug && a && s.log("confirm", e, r), t.progress(e), n++, n === i && t.complete(), !0
            }
            var t = this, n = 0, i = this.images.length;
            if (this.hasAnyBroken = !1, !i)
                return this.complete(), void 0;
            for (var r = 0; i > r; r++) {
                var o = this.images[r];
                o.on("confirm", e), o.check()
            }
        }, r.prototype.progress = function (e) {
            this.hasAnyBroken = this.hasAnyBroken || !e.isLoaded;
            var t = this;
            setTimeout(function () {
                t.emit("progress", t, e), t.jqDeferred && t.jqDeferred.notify(t, e)
            })
        }, r.prototype.complete = function () {
            var e = this.hasAnyBroken ? "fail" : "done";
            this.isComplete = !0;
            var t = this;
            setTimeout(function () {
                if (t.emit(e, t), t.emit("always", t), t.jqDeferred) {
                    var n = t.hasAnyBroken ? "reject" : "resolve";
                    t.jqDeferred[n](t)
                }
            })
        }, o && (o.fn.imagesLoaded = function (e, t) {
            var n = new r(this, e, t);
            return n.jqDeferred.promise(o(this))
        });
        var f = {};
        return c.prototype = new e, c.prototype.check = function () {
            var e = f[this.img.src];
            if (e)
                return this.useCached(e), void 0;
            if (f[this.img.src] = this, this.img.complete && void 0 !== this.img.naturalWidth)
                return this.confirm(0 !== this.img.naturalWidth, "naturalWidth"), void 0;
            var t = this.proxyImage = new Image;
            n.bind(t, "load", this), n.bind(t, "error", this), t.src = this.img.src
        }, c.prototype.useCached = function (e) {
            if (e.isConfirmed)
                this.confirm(e.isLoaded, "cached was confirmed");
            else {
                var t = this;
                e.on("confirm", function (e) {
                    return t.confirm(e.isLoaded, "cache emitted confirmed"), !0
                })
            }
        }, c.prototype.confirm = function (e, t) {
            this.isConfirmed = !0, this.isLoaded = e, this.emit("confirm", this, t)
        }, c.prototype.handleEvent = function (e) {
            var t = "on" + e.type;
            this[t] && this[t](e)
        }, c.prototype.onload = function () {
            this.confirm(!0, "onload"), this.unbindProxyEvents()
        }, c.prototype.onerror = function () {
            this.confirm(!1, "onerror"), this.unbindProxyEvents()
        }, c.prototype.unbindProxyEvents = function () {
            n.unbind(this.proxyImage, "load", this), n.unbind(this.proxyImage, "error", this)
        }, r
    }
    var o = e.jQuery, s = e.console, a = s !== void 0, c = Object.prototype.toString;
    "function" == typeof define && define.amd ? define(["eventEmitter/EventEmitter", "eventie/eventie"], r) : e.imagesLoaded = r(e.EventEmitter, e.eventie)
}(window);

/*jshint undef: true */
/*global jQuery: true */


// Init style shamelessly stolen from jQuery http://jquery.com
var Froogaloop = (function () {
    // Define a local copy of Froogaloop
    function Froogaloop(iframe) {
        // The Froogaloop object is actually just the init constructor
        return new Froogaloop.fn.init(iframe);
    }

    var eventCallbacks = {},
            hasWindowEvent = false,
            isReady = false,
            slice = Array.prototype.slice,
            playerDomain = '';

    Froogaloop.fn = Froogaloop.prototype = {
        element: null,
        init: function (iframe) {
            if (typeof iframe === "string") {
                iframe = document.getElementById(iframe);
            }

            this.element = iframe;

            // Register message event listeners
            playerDomain = getDomainFromUrl(this.element.getAttribute('src'));

            return this;
        },
        /*
         * Calls a function to act upon the player.
         *
         * @param {string} method The name of the Javascript API method to call. Eg: 'play'.
         * @param {Array|Function} valueOrCallback params Array of parameters to pass when calling an API method
         *                                or callback function when the method returns a value.
         */
        api: function (method, valueOrCallback) {
            if (!this.element || !method) {
                return false;
            }

            var self = this,
                    element = self.element,
                    target_id = element.id !== '' ? element.id : null,
                    params = !isFunction(valueOrCallback) ? valueOrCallback : null,
                    callback = isFunction(valueOrCallback) ? valueOrCallback : null;

            // Store the callback for get functions
            if (callback) {
                storeCallback(method, callback, target_id);
            }

            postMessage(method, params, element);
            return self;
        },
        /*
         * Registers an event listener and a callback function that gets called when the event fires.
         *
         * @param eventName (String): Name of the event to listen for.
         * @param callback (Function): Function that should be called when the event fires.
         */
        addEvent: function (eventName, callback) {
            if (!this.element) {
                return false;
            }

            var self = this,
                    element = self.element,
                    target_id = element.id !== '' ? element.id : null;


            storeCallback(eventName, callback, target_id);

            // The ready event is not registered via postMessage. It fires regardless.
            if (eventName != 'ready') {
                postMessage('addEventListener', eventName, element);
            } else if (eventName == 'ready' && isReady) {
                callback.call(null, target_id);
            }

            return self;
        },
        /*
         * Unregisters an event listener that gets called when the event fires.
         *
         * @param eventName (String): Name of the event to stop listening for.
         */
        removeEvent: function (eventName) {
            if (!this.element) {
                return false;
            }

            var self = this,
                    element = self.element,
                    target_id = element.id !== '' ? element.id : null,
                    removed = removeCallback(eventName, target_id);

            // The ready event is not registered
            if (eventName != 'ready' && removed) {
                postMessage('removeEventListener', eventName, element);
            }
        }
    };

    /**
     * Handles posting a message to the parent window.
     *
     * @param method (String): name of the method to call inside the player. For api calls
     * this is the name of the api method (api_play or api_pause) while for events this method
     * is api_addEventListener.
     * @param params (Object or Array): List of parameters to submit to the method. Can be either
     * a single param or an array list of parameters.
     * @param target (HTMLElement): Target iframe to post the message to.
     */
    function postMessage(method, params, target) {
        if (!target.contentWindow.postMessage) {
            return false;
        }

        var url = target.getAttribute('src').split('?')[0],
                data = JSON.stringify({
                    method: method,
                    value: params
                });

        if (url.substr(0, 2) === '//') {
            url = window.location.protocol + url;
        }

        target.contentWindow.postMessage(data, url);
    }

    /**
     * Event that fires whenever the window receives a message from its parent
     * via window.postMessage.
     */
    function onMessageReceived(event) {
        var data, method;

        try {
            data = JSON.parse(event.data);
            method = data.event || data.method;
        } catch (e) {
            //fail silently... like a ninja!
        }

        if (method == 'ready' && !isReady) {
            isReady = true;
        }

        // Handles messages from moogaloop only
        if (event.origin != playerDomain) {
            return false;
        }

        var value = data.value,
                eventData = data.data,
                target_id = target_id === '' ? null : data.player_id,
                callback = getCallback(method, target_id),
                params = [];

        if (!callback) {
            return false;
        }

        if (value !== undefined) {
            params.push(value);
        }

        if (eventData) {
            params.push(eventData);
        }

        if (target_id) {
            params.push(target_id);
        }

        return params.length > 0 ? callback.apply(null, params) : callback.call();
    }


    /**
     * Stores submitted callbacks for each iframe being tracked and each
     * event for that iframe.
     *
     * @param eventName (String): Name of the event. Eg. api_onPlay
     * @param callback (Function): Function that should get executed when the
     * event is fired.
     * @param target_id (String) [Optional]: If handling more than one iframe then
     * it stores the different callbacks for different iframes based on the iframe's
     * id.
     */
    function storeCallback(eventName, callback, target_id) {
        if (target_id) {
            if (!eventCallbacks[target_id]) {
                eventCallbacks[target_id] = {};
            }
            eventCallbacks[target_id][eventName] = callback;
        } else {
            eventCallbacks[eventName] = callback;
        }
    }

    /**
     * Retrieves stored callbacks.
     */
    function getCallback(eventName, target_id) {
        if (target_id) {
            return eventCallbacks[target_id][eventName];
        } else {
            return eventCallbacks[eventName];
        }
    }

    function removeCallback(eventName, target_id) {
        if (target_id && eventCallbacks[target_id]) {
            if (!eventCallbacks[target_id][eventName]) {
                return false;
            }
            eventCallbacks[target_id][eventName] = null;
        } else {
            if (!eventCallbacks[eventName]) {
                return false;
            }
            eventCallbacks[eventName] = null;
        }

        return true;
    }

    /**
     * Returns a domain's root domain.
     * Eg. returns http://vimeo.com when http://vimeo.com/channels is sbumitted
     *
     * @param url (String): Url to test against.
     * @return url (String): Root domain of submitted url
     */
    function getDomainFromUrl(url) {
        if (url.substr(0, 2) === '//') {
            url = window.location.protocol + url;
        }

        var url_pieces = url.split('/'),
                domain_str = '';

        for (var i = 0, length = url_pieces.length; i < length; i++) {
            if (i < 3) {
                domain_str += url_pieces[i];
            } else {
                break;
            }
            if (i < 2) {
                domain_str += '/';
            }
        }

        return domain_str;
    }

    function isFunction(obj) {
        return !!(obj && obj.constructor && obj.call && obj.apply);
    }

    function isArray(obj) {
        return toString.call(obj) === '[object Array]';
    }

    // Give the init function the Froogaloop prototype for later instantiation
    Froogaloop.fn.init.prototype = Froogaloop.fn;

    // Listens for the message event.
    // W3C
    if (window.addEventListener) {
        window.addEventListener('message', onMessageReceived, false);
    }
    // IE
    else {
        window.attachEvent('onmessage', onMessageReceived);
    }

    // Expose froogaloop to the global object
    return (window.Froogaloop = window.$f = Froogaloop);

})();

/**
 * WooCommerce Quanity buttons add-back
 */

jQuery(function ($) {
    if (typeof js_local_vars.woocommerce_23 !== 'undefined') {
        var $testProp = $('div.quantity:not(.buttons_added), td.quantity:not(.buttons_added)').find('qty');
        if ($testProp && $testProp.prop('type') != 'date') {
            // Quantity buttons
            //$('div.quantity:not(.buttons_added), td.quantity:not(.buttons_added)').addClass('buttons_added').append('<input type="button" value="+" class="plus" />').prepend('<input type="button" value="-" class="minus" />');

            // Target quantity inputs on product pages
            $('input.qty:not(.product-quantity input.qty)').each(function () {

                var min = parseFloat($(this).attr('min'));

                if (min && min > 0 && parseFloat($(this).val()) < min) {
                    $(this).val(min);
                }
            });

            $(document).on('click', '.plus, .minus', function () {

                // Get values
                var $qty = $(this).closest('.quantity').find('.qty'),
                        currentVal = parseFloat($qty.val()),
                        max = parseFloat($qty.attr('max')),
                        min = parseFloat($qty.attr('min')),
                        step = $qty.attr('step');

                // Format values
                if (!currentVal || currentVal === '' || currentVal === 'NaN')
                    currentVal = 0;
                if (max === '' || max === 'NaN')
                    max = '';
                if (min === '' || min === 'NaN')
                    min = 0;
                if (step === 'any' || step === '' || step === undefined || parseFloat(step) === 'NaN')
                    step = 1;

                // Change the value
                if ($(this).is('.plus')) {

                    if (max && (max == currentVal || currentVal > max)) {
                        $qty.val(max);
                    } else {
                        $qty.val(currentVal + parseFloat(step));
                    }

                } else {

                    if (min && (min == currentVal || currentVal < min)) {
                        $qty.val(min);
                    } else if (currentVal > 0) {
                        $qty.val(currentVal - parseFloat(step));
                    }

                }

                // Trigger change event
                $qty.trigger('change');
            });
        }
    }
});

/**
 * Isotope v1.5.25
 * An exquisite jQuery plugin for magical layouts
 * http://isotope.metafizzy.co
 *
 * Commercial use requires one-time purchase of a commercial license
 * http://isotope.metafizzy.co/docs/license.html
 *
 * Non-commercial use is licensed under the MIT License
 *
 * Copyright 2013 Metafizzy
 */

/*jshint asi: true, browser: true, curly: true, eqeqeq: true, forin: false, immed: false, newcap: true, noempty: true, strict: true, undef: true */
/*global jQuery: false */

(function (window, $, undefined) {

    'use strict';

    // get global vars
    var document = window.document;
    var Modernizr = window.Modernizr;

    // helper function
    var capitalize = function (str) {
        return str.charAt(0).toUpperCase() + str.slice(1);
    };

    // ========================= getStyleProperty by kangax ===============================
    // http://perfectionkills.com/feature-testing-css-properties/

    var prefixes = 'Moz Webkit O Ms'.split(' ');

    var getStyleProperty = function (propName) {
        var style = document.documentElement.style,
                prefixed;

        // test standard property first
        if (typeof style[propName] === 'string') {
            return propName;
        }

        // capitalize
        propName = capitalize(propName);

        // test vendor specific properties
        for (var i = 0, len = prefixes.length; i < len; i++) {
            prefixed = prefixes[i] + propName;
            if (typeof style[ prefixed ] === 'string') {
                return prefixed;
            }
        }
    };

    var transformProp = getStyleProperty('transform'),
            transitionProp = getStyleProperty('transitionProperty');




// **********************  Portfolio  ****************************
// ========================= Isotope ===============================


    // our "Widget" object constructor
    $.Isotope = function (options, element, callback) {
        this.element = $(element);

        this._create(options);
        this._init(callback);
    };

    // styles of container element we want to keep track of
    var isoContainerStyles = ['width', 'height'];

    var $window = $(window);

    $.Isotope.settings = {
        resizable: true,
        layoutMode: 'masonry',
        containerClass: 'isotope',
        itemClass: 'isotope-item',
        hiddenClass: 'isotope-hidden',
        hiddenStyle: {opacity: 0, scale: 0.001},
        visibleStyle: {opacity: 1, scale: 1},
        containerStyle: {
            position: 'relative',
            overflow: 'hidden'
        },
        animationEngine: 'best-available',
        animationOptions: {
            queue: false,
            duration: 800
        },
        sortBy: 'original-order',
        sortAscending: true,
        resizesContainer: true,
        transformsEnabled: true,
        itemPositionDataEnabled: false
    };

    $.Isotope.prototype = {
        // sets up widget
        _create: function (options) {

            this.options = $.extend({}, $.Isotope.settings, options);

            this.styleQueue = [];
            this.elemCount = 0;

            // get original styles in case we re-apply them in .destroy()
            var elemStyle = this.element[0].style;
            this.originalStyle = {};
            // keep track of container styles
            var containerStyles = isoContainerStyles.slice(0);
            for (var prop in this.options.containerStyle) {
                containerStyles.push(prop);
            }
            for (var i = 0, len = containerStyles.length; i < len; i++) {
                prop = containerStyles[i];
                this.originalStyle[ prop ] = elemStyle[ prop ] || '';
            }
            // apply container style from options
            this.element.css(this.options.containerStyle);

            this._updateAnimationEngine();
            this._updateUsingTransforms();

            // sorting
            var originalOrderSorter = {
                'original-order': function ($elem, instance) {
                    instance.elemCount++;
                    return instance.elemCount;
                },
                random: function () {
                    return Math.random();
                }
            };

            this.options.getSortData = $.extend(this.options.getSortData, originalOrderSorter);

            // need to get atoms
            this.reloadItems();

            // get top left position of where the bricks should be
            this.offset = {
                left: parseInt((this.element.css('padding-left') || 0), 10),
                top: parseInt((this.element.css('padding-top') || 0), 10)
            };

            // add isotope class first time around
            var instance = this;
            setTimeout(function () {
                instance.element.addClass(instance.options.containerClass);
            }, 0);

            // bind resize method
            if (this.options.resizable) {
                $window.bind('smartresize.isotope', function () {
                    instance.resize();
                });
            }

            // dismiss all click events from hidden events
            this.element.delegate('.' + this.options.hiddenClass, 'click', function () {
                return false;
            });

        },
        _getAtoms: function ($elems) {
            var selector = this.options.itemSelector,
                    // filter & find
                    $atoms = selector ? $elems.filter(selector).add($elems.find(selector)) : $elems,
                    // base style for atoms
                    atomStyle = {position: 'absolute'};

            // filter out text nodes
            $atoms = $atoms.filter(function (i, atom) {
                return atom.nodeType === 1;
            });

            if (this.usingTransforms) {
                atomStyle.left = 0;
                atomStyle.top = 0;
            }

            $atoms.css(atomStyle).addClass(this.options.itemClass);

            this.updateSortData($atoms, true);

            return $atoms;
        },
        // _init fires when your instance is first created
        // (from the constructor above), and when you
        // attempt to initialize the widget again (by the bridge)
        // after it has already been initialized.
        _init: function (callback) {

            this.$filteredAtoms = this._filter(this.$allAtoms);
            this._sort();
            this.reLayout(callback);

        },
        option: function (opts) {
            // change options AFTER initialization:
            // signature: $('#foo').bar({ cool:false });
            if ($.isPlainObject(opts)) {
                this.options = $.extend(true, this.options, opts);

                // trigger _updateOptionName if it exists
                var updateOptionFn;
                for (var optionName in opts) {
                    updateOptionFn = '_update' + capitalize(optionName);
                    if (this[ updateOptionFn ]) {
                        this[ updateOptionFn ]();
                    }
                }
            }
        },
        // ====================== updaters ====================== //
        // kind of like setters

        _updateAnimationEngine: function () {
            var animationEngine = this.options.animationEngine.toLowerCase().replace(/[ _\-]/g, '');
            var isUsingJQueryAnimation;
            // set applyStyleFnName
            switch (animationEngine) {
                case 'css' :
                case 'none' :
                    isUsingJQueryAnimation = false;
                    break;
                case 'jquery' :
                    isUsingJQueryAnimation = true;
                    break;
                default : // best available
                    isUsingJQueryAnimation = !Modernizr.csstransitions;
            }
            this.isUsingJQueryAnimation = isUsingJQueryAnimation;
            this._updateUsingTransforms();
        },
        _updateTransformsEnabled: function () {
            this._updateUsingTransforms();
        },
        _updateUsingTransforms: function () {
            var usingTransforms = this.usingTransforms = this.options.transformsEnabled &&
                    Modernizr.csstransforms && Modernizr.csstransitions && !this.isUsingJQueryAnimation;

            // prevent scales when transforms are disabled
            if (!usingTransforms) {
                delete this.options.hiddenStyle.scale;
                delete this.options.visibleStyle.scale;
            }

            this.getPositionStyles = usingTransforms ? this._translate : this._positionAbs;
        },
        // ====================== Filtering ======================

        _filter: function ($atoms) {
            var filter = this.options.filter === '' ? '*' : this.options.filter;

            if (!filter) {
                return $atoms;
            }

            var hiddenClass = this.options.hiddenClass,
                    hiddenSelector = '.' + hiddenClass,
                    $hiddenAtoms = $atoms.filter(hiddenSelector),
                    $atomsToShow = $hiddenAtoms;

            if (filter !== '*') {
                $atomsToShow = $hiddenAtoms.filter(filter);
                var $atomsToHide = $atoms.not(hiddenSelector).not(filter).addClass(hiddenClass);
                this.styleQueue.push({$el: $atomsToHide, style: this.options.hiddenStyle});
            }

            this.styleQueue.push({$el: $atomsToShow, style: this.options.visibleStyle});
            $atomsToShow.removeClass(hiddenClass);

            return $atoms.filter(filter);
        },
        // ====================== Sorting ======================

        updateSortData: function ($atoms, isIncrementingElemCount) {
            var instance = this,
                    getSortData = this.options.getSortData,
                    $this, sortData;
            $atoms.each(function () {
                $this = $(this);
                sortData = {};
                // get value for sort data based on fn( $elem ) passed in
                for (var key in getSortData) {
                    if (!isIncrementingElemCount && key === 'original-order') {
                        // keep original order original
                        sortData[ key ] = $.data(this, 'isotope-sort-data')[ key ];
                    } else {
                        sortData[ key ] = getSortData[ key ]($this, instance);
                    }
                }
                // apply sort data to element
                $.data(this, 'isotope-sort-data', sortData);
            });
        },
        // used on all the filtered atoms
        _sort: function () {

            var sortBy = this.options.sortBy,
                    getSorter = this._getSorter,
                    sortDir = this.options.sortAscending ? 1 : -1,
                    sortFn = function (alpha, beta) {
                        var a = getSorter(alpha, sortBy),
                                b = getSorter(beta, sortBy);
                        // fall back to original order if data matches
                        if (a === b && sortBy !== 'original-order') {
                            a = getSorter(alpha, 'original-order');
                            b = getSorter(beta, 'original-order');
                        }
                        return ((a > b) ? 1 : (a < b) ? -1 : 0) * sortDir;
                    };

            this.$filteredAtoms.sort(sortFn);
        },
        _getSorter: function (elem, sortBy) {
            return $.data(elem, 'isotope-sort-data')[ sortBy ];
        },
        // ====================== Layout Helpers ======================

        _translate: function (x, y) {
            return {translate: [x, y]};
        },
        _positionAbs: function (x, y) {
            return {left: x, top: y};
        },
        _pushPosition: function ($elem, x, y) {
            x = Math.round(x + this.offset.left);
            y = Math.round(y + this.offset.top);
            var position = this.getPositionStyles(x, y);
            this.styleQueue.push({$el: $elem, style: position});
            if (this.options.itemPositionDataEnabled) {
                $elem.data('isotope-item-position', {x: x, y: y});
            }
        },
        // ====================== General Layout ======================

        // used on collection of atoms (should be filtered, and sorted before )
        // accepts atoms-to-be-laid-out to start with
        layout: function ($elems, callback) {

            var layoutMode = this.options.layoutMode;

            // layout logic
            this[ '_' + layoutMode + 'Layout' ]($elems);

            // set the size of the container
            if (this.options.resizesContainer) {
                var containerStyle = this[ '_' + layoutMode + 'GetContainerSize' ]();
                this.styleQueue.push({$el: this.element, style: containerStyle});
            }

            this._processStyleQueue($elems, callback);

            this.isLaidOut = true;
        },
        _processStyleQueue: function ($elems, callback) {
            // are we animating the layout arrangement?
            // use plugin-ish syntax for css or animate
            var styleFn = !this.isLaidOut ? 'css' : (
                    this.isUsingJQueryAnimation ? 'animate' : 'css'
                    ),
                    animOpts = this.options.animationOptions,
                    onLayout = this.options.onLayout,
                    objStyleFn, processor,
                    triggerCallbackNow, callbackFn;

            // default styleQueue processor, may be overwritten down below
            processor = function (i, obj) {
                obj.$el[ styleFn ](obj.style, animOpts);
            };

            if (this._isInserting && this.isUsingJQueryAnimation) {
                // if using styleQueue to insert items
                processor = function (i, obj) {
                    // only animate if it not being inserted
                    objStyleFn = obj.$el.hasClass('no-transition') ? 'css' : styleFn;
                    obj.$el[ objStyleFn ](obj.style, animOpts);
                };

            } else if (callback || onLayout || animOpts.complete) {
                // has callback
                var isCallbackTriggered = false,
                        // array of possible callbacks to trigger
                        callbacks = [callback, onLayout, animOpts.complete],
                        instance = this;
                triggerCallbackNow = true;
                // trigger callback only once
                callbackFn = function () {
                    if (isCallbackTriggered) {
                        return;
                    }
                    var hollaback;
                    for (var i = 0, len = callbacks.length; i < len; i++) {
                        hollaback = callbacks[i];
                        if (typeof hollaback === 'function') {
                            hollaback.call(instance.element, $elems, instance);
                        }
                    }
                    isCallbackTriggered = true;
                };

                if (this.isUsingJQueryAnimation && styleFn === 'animate') {
                    // add callback to animation options
                    animOpts.complete = callbackFn;
                    triggerCallbackNow = false;

                } else if (Modernizr.csstransitions) {
                    // detect if first item has transition
                    var i = 0,
                            firstItem = this.styleQueue[0],
                            testElem = firstItem && firstItem.$el,
                            styleObj;
                    // get first non-empty jQ object
                    while (!testElem || !testElem.length) {
                        styleObj = this.styleQueue[ i++ ];
                        // HACK: sometimes styleQueue[i] is undefined
                        if (!styleObj) {
                            return;
                        }
                        testElem = styleObj.$el;
                    }
                    // get transition duration of the first element in that object
                    // yeah, this is inexact
                    var duration = parseFloat(getComputedStyle(testElem[0])[ transitionDurProp ]);
                    if (duration > 0) {
                        processor = function (i, obj) {
                            obj.$el[ styleFn ](obj.style, animOpts)
                                    // trigger callback at transition end
                                    .one(transitionEndEvent, callbackFn);
                        };
                        triggerCallbackNow = false;
                    }
                }
            }

            // process styleQueue
            $.each(this.styleQueue, processor);

            if (triggerCallbackNow) {
                callbackFn();
            }

            // clear out queue for next time
            this.styleQueue = [];
        },
        resize: function () {
            if (this[ '_' + this.options.layoutMode + 'ResizeChanged' ]()) {
                this.reLayout();
            }
        },
        reLayout: function (callback) {

            this[ '_' + this.options.layoutMode + 'Reset' ]();
            this.layout(this.$filteredAtoms, callback);

        },
        // ====================== Convenience methods ======================

        // ====================== Adding items ======================

        // adds a jQuery object of items to a isotope container
        addItems: function ($content, callback) {
            var $newAtoms = this._getAtoms($content);
            // add new atoms to atoms pools
            this.$allAtoms = this.$allAtoms.add($newAtoms);

            if (callback) {
                callback($newAtoms);
            }
        },
        // convienence method for adding elements properly to any layout
        // positions items, hides them, then animates them back in <--- very sezzy
        insert: function ($content, callback) {
            // position items
            this.element.append($content);

            var instance = this;
            this.addItems($content, function ($newAtoms) {
                var $newFilteredAtoms = instance._filter($newAtoms);
                instance._addHideAppended($newFilteredAtoms);
                instance._sort();
                instance.reLayout();
                instance._revealAppended($newFilteredAtoms, callback);
            });

        },
        // convienence method for working with Infinite Scroll
        appended: function ($content, callback) {
            var instance = this;
            this.addItems($content, function ($newAtoms) {
                instance._addHideAppended($newAtoms);
                instance.layout($newAtoms);
                instance._revealAppended($newAtoms, callback);
            });
        },
        // adds new atoms, then hides them before positioning
        _addHideAppended: function ($newAtoms) {
            this.$filteredAtoms = this.$filteredAtoms.add($newAtoms);
            $newAtoms.addClass('no-transition');

            this._isInserting = true;

            // apply hidden styles
            this.styleQueue.push({$el: $newAtoms, style: this.options.hiddenStyle});
        },
        // sets visible style on new atoms
        _revealAppended: function ($newAtoms, callback) {
            var instance = this;
            // apply visible style after a sec
            setTimeout(function () {
                // enable animation
                $newAtoms.removeClass('no-transition');
                // reveal newly inserted filtered elements
                instance.styleQueue.push({$el: $newAtoms, style: instance.options.visibleStyle});
                instance._isInserting = false;
                instance._processStyleQueue($newAtoms, callback);
            }, 10);
        },
        // gathers all atoms
        reloadItems: function () {
            this.$allAtoms = this._getAtoms(this.element.children());
        },
        // removes elements from Isotope widget
        remove: function ($content, callback) {
            // remove elements immediately from Isotope instance
            this.$allAtoms = this.$allAtoms.not($content);
            this.$filteredAtoms = this.$filteredAtoms.not($content);
            // remove() as a callback, for after transition / animation
            var instance = this;
            var removeContent = function () {
                $content.remove();
                if (callback) {
                    callback.call(instance.element);
                }
            };

            if ($content.filter(':not(.' + this.options.hiddenClass + ')').length) {
                // if any non-hidden content needs to be removed
                this.styleQueue.push({$el: $content, style: this.options.hiddenStyle});
                this._sort();
                this.reLayout(removeContent);
            } else {
                // remove it now
                removeContent();
            }

        },
        shuffle: function (callback) {
            this.updateSortData(this.$allAtoms);
            this.options.sortBy = 'random';
            this._sort();
            this.reLayout(callback);
        },
        // destroys widget, returns elements and container back (close) to original style
        destroy: function () {

            var usingTransforms = this.usingTransforms;
            var options = this.options;

            this.$allAtoms
                    .removeClass(options.hiddenClass + ' ' + options.itemClass)
                    .each(function () {
                        var style = this.style;
                        style.position = '';
                        style.top = '';
                        style.left = '';
                        style.opacity = '';
                        if (usingTransforms) {
                            style[ transformProp ] = '';
                        }
                    });

            // re-apply saved container styles
            var elemStyle = this.element[0].style;
            for (var prop in this.originalStyle) {
                elemStyle[ prop ] = this.originalStyle[ prop ];
            }

            this.element
                    .unbind('.isotope')
                    .undelegate('.' + options.hiddenClass, 'click')
                    .removeClass(options.containerClass)
                    .removeData('isotope');

            $window.unbind('.isotope');

        },
        // ====================== LAYOUTS ======================

        // calculates number of rows or columns
        // requires columnWidth or rowHeight to be set on namespaced object
        // i.e. this.masonry.columnWidth = 200
        _getSegments: function (isRows) {
            var namespace = this.options.layoutMode,
                    measure = isRows ? 'rowHeight' : 'columnWidth',
                    size = isRows ? 'height' : 'width',
                    segmentsName = isRows ? 'rows' : 'cols',
                    containerSize = this.element[ size ](),
                    segments,
                    // i.e. options.masonry && options.masonry.columnWidth
                    segmentSize = this.options[ namespace ] && this.options[ namespace ][ measure ] ||
                    // or use the size of the first item, i.e. outerWidth
                    this.$filteredAtoms[ 'outer' + capitalize(size) ](true) ||
                    // if there's no items, use size of container
                    containerSize;

            segments = Math.floor(containerSize / segmentSize);
            segments = Math.max(segments, 1);

            // i.e. this.masonry.cols = ....
            this[ namespace ][ segmentsName ] = segments;
            // i.e. this.masonry.columnWidth = ...
            this[ namespace ][ measure ] = segmentSize;

        },
        _checkIfSegmentsChanged: function (isRows) {
            var namespace = this.options.layoutMode,
                    segmentsName = isRows ? 'rows' : 'cols',
                    prevSegments = this[ namespace ][ segmentsName ];
            // update cols/rows
            this._getSegments(isRows);
            // return if updated cols/rows is not equal to previous
            return (this[ namespace ][ segmentsName ] !== prevSegments);
        },
        // ====================== Masonry ======================

        _masonryReset: function () {
            // layout-specific props
            this.masonry = {};
            // FIXME shouldn't have to call this again
            this._getSegments();
            var i = this.masonry.cols;
            this.masonry.colYs = [];
            while (i--) {
                this.masonry.colYs.push(0);
            }
        },
        _masonryLayout: function ($elems) {
            var instance = this,
                    props = instance.masonry;
            $elems.each(function () {
                var $this = $(this),
                        //how many columns does this brick span
                        colSpan = Math.ceil($this.outerWidth(true) / props.columnWidth);
                colSpan = Math.min(colSpan, props.cols);

                if (colSpan === 1) {
                    // if brick spans only one column, just like singleMode
                    instance._masonryPlaceBrick($this, props.colYs);
                } else {
                    // brick spans more than one column
                    // how many different places could this brick fit horizontally
                    var groupCount = props.cols + 1 - colSpan,
                            groupY = [],
                            groupColY,
                            i;

                    // for each group potential horizontal position
                    for (i = 0; i < groupCount; i++) {
                        // make an array of colY values for that one group
                        groupColY = props.colYs.slice(i, i + colSpan);
                        // and get the max value of the array
                        groupY[i] = Math.max.apply(Math, groupColY);
                    }

                    instance._masonryPlaceBrick($this, groupY);
                }
            });
        },
        // worker method that places brick in the columnSet
        //   with the the minY
        _masonryPlaceBrick: function ($brick, setY) {
            // get the minimum Y value from the columns
            var minimumY = Math.min.apply(Math, setY),
                    shortCol = 0;

            // Find index of short column, the first from the left
            for (var i = 0, len = setY.length; i < len; i++) {
                if (setY[i] === minimumY) {
                    shortCol = i;
                    break;
                }
            }

            // position the brick
            var x = this.masonry.columnWidth * shortCol,
                    y = minimumY;
            this._pushPosition($brick, x, y);

            // apply setHeight to necessary columns
            var setHeight = minimumY + $brick.outerHeight(true),
                    setSpan = this.masonry.cols + 1 - len;
            for (i = 0; i < setSpan; i++) {
                this.masonry.colYs[ shortCol + i ] = setHeight;
            }

        },
        _masonryGetContainerSize: function () {
            var containerHeight = Math.max.apply(Math, this.masonry.colYs);
            return {height: containerHeight};
        },
        _masonryResizeChanged: function () {
            return this._checkIfSegmentsChanged();
        },
        // ====================== fitRows ======================

        _fitRowsReset: function () {
            this.fitRows = {
                x: 0,
                y: 0,
                height: 0
            };
        },
        _fitRowsLayout: function ($elems) {
            var instance = this,
                    containerWidth = this.element.width(),
                    props = this.fitRows;

            $elems.each(function () {
                var $this = $(this),
                        atomW = $this.outerWidth(true),
                        atomH = $this.outerHeight(true);

                if (props.x !== 0 && atomW + props.x > containerWidth) {
                    // if this element cannot fit in the current row
                    props.x = 0;
                    props.y = props.height;
                }

                // position the atom
                instance._pushPosition($this, props.x, props.y);

                props.height = Math.max(props.y + atomH, props.height);
                props.x += atomW;

            });
        },
        _fitRowsGetContainerSize: function () {
            return {height: this.fitRows.height};
        },
        _fitRowsResizeChanged: function () {
            return true;
        },
        // ====================== cellsByRow ======================

        _cellsByRowReset: function () {
            this.cellsByRow = {
                index: 0
            };
            // get this.cellsByRow.columnWidth
            this._getSegments();
            // get this.cellsByRow.rowHeight
            this._getSegments(true);
        },
        _cellsByRowLayout: function ($elems) {
            var instance = this,
                    props = this.cellsByRow;
            $elems.each(function () {
                var $this = $(this),
                        col = props.index % props.cols,
                        row = Math.floor(props.index / props.cols),
                        x = (col + 0.5) * props.columnWidth - $this.outerWidth(true) / 2,
                        y = (row + 0.5) * props.rowHeight - $this.outerHeight(true) / 2;
                instance._pushPosition($this, x, y);
                props.index++;
            });
        },
        _cellsByRowGetContainerSize: function () {
            return {height: Math.ceil(this.$filteredAtoms.length / this.cellsByRow.cols) * this.cellsByRow.rowHeight + this.offset.top};
        },
        _cellsByRowResizeChanged: function () {
            return this._checkIfSegmentsChanged();
        },
        // ====================== straightDown ======================

        _straightDownReset: function () {
            this.straightDown = {
                y: 0
            };
        },
        _straightDownLayout: function ($elems) {
            var instance = this;
            $elems.each(function (i) {
                var $this = $(this);
                instance._pushPosition($this, 0, instance.straightDown.y);
                instance.straightDown.y += $this.outerHeight(true);
            });
        },
        _straightDownGetContainerSize: function () {
            return {height: this.straightDown.y};
        },
        _straightDownResizeChanged: function () {
            return true;
        },
        // ====================== masonryHorizontal ======================

        _masonryHorizontalReset: function () {
            // layout-specific props
            this.masonryHorizontal = {};
            // FIXME shouldn't have to call this again
            this._getSegments(true);
            var i = this.masonryHorizontal.rows;
            this.masonryHorizontal.rowXs = [];
            while (i--) {
                this.masonryHorizontal.rowXs.push(0);
            }
        },
        _masonryHorizontalLayout: function ($elems) {
            var instance = this,
                    props = instance.masonryHorizontal;
            $elems.each(function () {
                var $this = $(this),
                        //how many rows does this brick span
                        rowSpan = Math.ceil($this.outerHeight(true) / props.rowHeight);
                rowSpan = Math.min(rowSpan, props.rows);

                if (rowSpan === 1) {
                    // if brick spans only one column, just like singleMode
                    instance._masonryHorizontalPlaceBrick($this, props.rowXs);
                } else {
                    // brick spans more than one row
                    // how many different places could this brick fit horizontally
                    var groupCount = props.rows + 1 - rowSpan,
                            groupX = [],
                            groupRowX, i;

                    // for each group potential horizontal position
                    for (i = 0; i < groupCount; i++) {
                        // make an array of colY values for that one group
                        groupRowX = props.rowXs.slice(i, i + rowSpan);
                        // and get the max value of the array
                        groupX[i] = Math.max.apply(Math, groupRowX);
                    }

                    instance._masonryHorizontalPlaceBrick($this, groupX);
                }
            });
        },
        _masonryHorizontalPlaceBrick: function ($brick, setX) {
            // get the minimum Y value from the columns
            var minimumX = Math.min.apply(Math, setX),
                    smallRow = 0;
            // Find index of smallest row, the first from the top
            for (var i = 0, len = setX.length; i < len; i++) {
                if (setX[i] === minimumX) {
                    smallRow = i;
                    break;
                }
            }

            // position the brick
            var x = minimumX,
                    y = this.masonryHorizontal.rowHeight * smallRow;
            this._pushPosition($brick, x, y);

            // apply setHeight to necessary columns
            var setWidth = minimumX + $brick.outerWidth(true),
                    setSpan = this.masonryHorizontal.rows + 1 - len;
            for (i = 0; i < setSpan; i++) {
                this.masonryHorizontal.rowXs[ smallRow + i ] = setWidth;
            }
        },
        _masonryHorizontalGetContainerSize: function () {
            var containerWidth = Math.max.apply(Math, this.masonryHorizontal.rowXs);
            return {width: containerWidth};
        },
        _masonryHorizontalResizeChanged: function () {
            return this._checkIfSegmentsChanged(true);
        },
        // ====================== fitColumns ======================

        _fitColumnsReset: function () {
            this.fitColumns = {
                x: 0,
                y: 0,
                width: 0
            };
        },
        _fitColumnsLayout: function ($elems) {
            var instance = this,
                    containerHeight = this.element.height(),
                    props = this.fitColumns;
            $elems.each(function () {
                var $this = $(this),
                        atomW = $this.outerWidth(true),
                        atomH = $this.outerHeight(true);

                if (props.y !== 0 && atomH + props.y > containerHeight) {
                    // if this element cannot fit in the current column
                    props.x = props.width;
                    props.y = 0;
                }

                // position the atom
                instance._pushPosition($this, props.x, props.y);

                props.width = Math.max(props.x + atomW, props.width);
                props.y += atomH;

            });
        },
        _fitColumnsGetContainerSize: function () {
            return {width: this.fitColumns.width};
        },
        _fitColumnsResizeChanged: function () {
            return true;
        },
        // ====================== cellsByColumn ======================

        _cellsByColumnReset: function () {
            this.cellsByColumn = {
                index: 0
            };
            // get this.cellsByColumn.columnWidth
            this._getSegments();
            // get this.cellsByColumn.rowHeight
            this._getSegments(true);
        },
        _cellsByColumnLayout: function ($elems) {
            var instance = this,
                    props = this.cellsByColumn;
            $elems.each(function () {
                var $this = $(this),
                        col = Math.floor(props.index / props.rows),
                        row = props.index % props.rows,
                        x = (col + 0.5) * props.columnWidth - $this.outerWidth(true) / 2,
                        y = (row + 0.5) * props.rowHeight - $this.outerHeight(true) / 2;
                instance._pushPosition($this, x, y);
                props.index++;
            });
        },
        _cellsByColumnGetContainerSize: function () {
            return {width: Math.ceil(this.$filteredAtoms.length / this.cellsByColumn.rows) * this.cellsByColumn.columnWidth};
        },
        _cellsByColumnResizeChanged: function () {
            return this._checkIfSegmentsChanged(true);
        },
        // ====================== straightAcross ======================

        _straightAcrossReset: function () {
            this.straightAcross = {
                x: 0
            };
        },
        _straightAcrossLayout: function ($elems) {
            var instance = this;
            $elems.each(function (i) {
                var $this = $(this);
                instance._pushPosition($this, instance.straightAcross.x, 0);
                instance.straightAcross.x += $this.outerWidth(true);
            });
        },
        _straightAcrossGetContainerSize: function () {
            return {width: this.straightAcross.x};
        },
        _straightAcrossResizeChanged: function () {
            return true;
        }

    };


// **********************  Portfolio  ****************************

    // =======================  Plugin bridge  ===============================
    // leverages data method to either create or return $.Isotope constructor
    // A bit from jQuery UI
    //   https://github.com/jquery/jquery-ui/blob/master/ui/jquery.ui.widget.js
    // A bit from jcarousel
    //   https://github.com/jsor/jcarousel/blob/master/lib/jquery.jcarousel.js

    $.fn.isotope = function (options, callback) {
        if (typeof options === 'string') {
            // call method
            var args = Array.prototype.slice.call(arguments, 1);

            this.each(function () {
                var instance = $.data(this, 'isotope');
                if (!instance) {
                    logError("cannot call methods on isotope prior to initialization; " +
                            "attempted to call method '" + options + "'");
                    return;
                }
                if (!$.isFunction(instance[options]) || options.charAt(0) === "_") {
                    logError("no such method '" + options + "' for isotope instance");
                    return;
                }
                // apply method
                instance[ options ].apply(instance, args);
            });
        } else {
            this.each(function () {
                var instance = $.data(this, 'isotope');
                if (instance) {
                    // apply options & init
                    instance.option(options);
                    instance._init(callback);
                } else {
                    // initialize new instance
                    $.data(this, 'isotope', new $.Isotope(options, this, callback));
                }
            });
        }
        // return jQuery object
        // so plugin methods do not have to
        return this;
    };

})(window, jQuery);

// **********************  Portfolio  ****************************
jQuery(document).ready(function () {
    jQuery('.portfolio-wrapper').hide();
    jQuery('.portfolio-tabs ').hide();
    jQuery('.faq-tabs ').hide();
    if (jQuery('.portfolio').length >= 1) {
        jQuery('#content').append('<div class="loading-container"><div class="loading-spinner"><div class="t4p-icon-repeat"></div><div class="loading-msg">' + js_local_vars.portfolio_loading_text + '</div>');
    }

});

jQuery(window).load(function () {
    if (jQuery().isotope) {
        // modified Isotope methods for gutters in masonry
        jQuery.Isotope.prototype._getMasonryGutterColumns = function () {
            var gutter = this.options.masonry && this.options.masonry.gutterWidth || 0;
            containerWidth = this.element.width();

            this.masonry.columnWidth = this.options.masonry && this.options.masonry.columnWidth ||
                    // or use the size of the first item
                    this.$filteredAtoms.outerWidth(true) ||
                    // if there's no items, use size of container
                    containerWidth;

            this.masonry.columnWidth += gutter;

            this.masonry.cols = Math.floor((containerWidth + gutter) / this.masonry.columnWidth);
            this.masonry.cols = Math.max(this.masonry.cols, 1);
        };

        jQuery.Isotope.prototype._masonryReset = function () {
            // layout-specific props
            this.masonry = {};
            // FIXME shouldn't have to call this again
            this._getMasonryGutterColumns();
            var i = this.masonry.cols;
            this.masonry.colYs = [];
            while (i--) {
                this.masonry.colYs.push(0);
            }
        };

        jQuery.Isotope.prototype._masonryResizeChanged = function () {
            var prevSegments = this.masonry.cols;
            // update cols/rows
            this._getMasonryGutterColumns();
            // return if updated cols/rows is not equal to previous
            return (this.masonry.cols !== prevSegments);
        };

        imagesLoaded(jQuery('.portfolio-one .portfolio-wrapper'), function () {
            jQuery('.portfolio-wrapper').fadeIn();
            jQuery('.portfolio-tabs').fadeIn();
            jQuery('.faq-tabs').fadeIn();
            jQuery('.loading-container').fadeOut();
            jQuery('.portfolio-one .portfolio-wrapper').isotope({
                // options
                itemSelector: '.portfolio-item',
                layoutMode: 'straightDown',
                transformsEnabled: false
            });
            jQuery('[data-spy="scroll"]').each(function () {
                var $spy = jQuery(this).scrollspy('refresh');
            });
        });

        imagesLoaded(jQuery('.portfolio-two .portfolio-wrapper, .portfolio-three .portfolio-wrapper, .portfolio-four .portfolio-wrapper'), function () {
            jQuery('.portfolio-wrapper').fadeIn();
            jQuery('.portfolio-tabs').fadeIn();
            jQuery('.loading-container').fadeOut();
            jQuery('.portfolio-two .portfolio-wrapper, .portfolio-three .portfolio-wrapper, .portfolio-four .portfolio-wrapper').isotope({
                // options
                itemSelector: '.portfolio-item',
                layoutMode: 'fitRows',
                transformsEnabled: false
            });
            jQuery('[data-spy="scroll"]').each(function () {
                var $spy = jQuery(this).scrollspy('refresh');
            });
        });

        var masonryContainer = jQuery('.portfolio-grid-mansory .portfolio-wrapper');
        imagesLoaded(masonryContainer, function () {
            jQuery('.portfolio-wrapper').fadeIn();
            jQuery('.portfolio-tabs').fadeIn();
            jQuery('.loading-container').fadeOut();
            var gridTwo = masonryContainer.parent().hasClass('portfolio-grid-2');
            var columns;
            if (gridTwo) {
                columns = 2;
            } else {
                columns = 3;
            }
            masonryContainer.isotope({
                // options
                itemSelector: '.portfolio-item',
                layoutMode: 'masonry',
                transformsEnabled: false,
                masonry: {columnWidth: masonryContainer.width() / columns}
            });
            jQuery('[data-spy="scroll"]').each(function () {
                var $spy = jQuery(this).scrollspy('refresh');
            });
        });

    }
    ;

    if (jQuery('.portfolio').length >= 1) {
        if (jQuery().isotope) {
            var gridwidth = (jQuery('.grid-layout').width() / 2) - 22;
            jQuery('.grid-layout .post').css('width', gridwidth);
            jQuery('.grid-layout').isotope({
                layoutMode: 'masonry',
                itemSelector: '.post',
                transformsEnabled: false,
                masonry: {
                    columnWidth: gridwidth,
                    gutterWidth: 40
                },
            });

            var gridwidth = (jQuery('.grid-layout-3').width() / 3) - 30;
            jQuery('.grid-layout-3 .post').css('width', gridwidth);
            jQuery('.grid-layout-3').isotope({
                layoutMode: 'masonry',
                itemSelector: '.post',
                transformsEnabled: false,
                masonry: {
                    columnWidth: gridwidth,
                    gutterWidth: 40
                }
            });

            var gridwidth = (jQuery('.grid-layout-4').width() / 4) - 35;
            jQuery('.grid-layout-4 .post').css('width', gridwidth);
            jQuery('.grid-layout-4').isotope({
                layoutMode: 'masonry',
                itemSelector: '.post',
                transformsEnabled: false,
                masonry: {
                    columnWidth: gridwidth,
                    gutterWidth: 40
                }
            });
        }
    }
});

// **********************  home content box style for mac and iphone  ****************************

if (navigator.userAgent.indexOf('Safari') != -1 && navigator.userAgent.indexOf('Chrome') == -1) {
    jQuery(".content-box .cntbox_btn").css({'display': 'block', 'position': 'relative', 'top': '0px', 'height': 'auto'});
}

var is_OSX = navigator.platform.match(/(Mac|iPhone|iPod|iPad)/i) ? true : false;
var is_iOS = navigator.platform.match(/(iPhone|iPod|iPad)/i) ? true : false;

var is_Mac = navigator.platform.toUpperCase().indexOf('MAC') >= 0;
var is_iPhone = navigator.platform == "iPhone";
var is_iPod = navigator.platform == "iPod";
var is_iPad = navigator.platform == "iPad";

//var oscheck= "Platform: " + navigator.platform;

if (is_OSX) {
    jQuery(".home-content-boxes .col-md-3.content-box, .home-content-boxes .col-md-4.content-box, .home-content-boxes .col-md-6.content-box").addClass('osmac');
}

// **********************  home content box button style for mac and iphone  ****************************
jQuery(window).load(function () {
    if (navigator.userAgent.indexOf('Safari') != -1 && navigator.userAgent.indexOf('Chrome') == -1) {


        function setHeight() {
            var heights1 = jQuery(".content-box p").map(function () {
                return jQuery(this).outerHeight();
            }).get();

            var heights2 = jQuery(".content-box h2").map(function () {
                return jQuery(this).outerHeight();
            }).get();

            var totalheights = [];
            for (var i = 0; i < heights1.length; i++)
            {
                totalheights.push(heights1[i] + heights2[i]);
            }

            maxHeight = Math.max.apply(null, totalheights);

            var btnpadding = jQuery.map(totalheights, function (value) {
                return maxHeight - value;
            });

            jQuery(".sbtn1").css('padding-top', btnpadding[0]);
            jQuery(".sbtn2").css('padding-top', btnpadding[1]);
            jQuery(".sbtn3").css('padding-top', btnpadding[2]);
            jQuery(".sbtn4").css('padding-top', btnpadding[3]);
        }
        ;
        setHeight();

        jQuery(window).resize(function () {
            var width = jQuery(window).width();
            if (width > '768') {
                setHeight();
            } else {
                jQuery(".sbtn1").css('padding-top', '0px');
                jQuery(".sbtn2").css('padding-top', '0px');
                jQuery(".sbtn3").css('padding-top', '0px');
                jQuery(".sbtn4").css('padding-top', '0px');
            }
        });
    }
});

/* add menu effect to WPML menu items */
jQuery(document).ready(function () {
    jQuery('.primary-menu .menu-item-language a,.sticky-header .menu-item-language a').each(function () {
        var el = jQuery(this);
        plan_text = el.text();
        if (jQuery(this).find('img').length) {
            img_src = jQuery(this).find('img').attr('src');
            jQuery(this).find('img').remove();
            el.html('<img src="' + img_src + '"> <span data-hover=" ' + plan_text + '"> ' + plan_text + '</span>');
        } else {
            el.html('<span data-hover="' + plan_text + '">' + plan_text + '</span>');
        }
    });
});

/* add menu effect to ubermenu items */
jQuery(document).ready(function () {

//for sticky header   
    if (jQuery('.sticky-header nav').children('.ubermenu').length > 0) {
        jQuery('.sticky-header nav').removeClass('link-effect');
    } else {
        jQuery('.sticky-header nav').addClass('link-effect');
    }

//for main header 
    if (jQuery('.primary-menu nav').children('.ubermenu').length > 0) {
        jQuery('.primary-menu nav').removeClass('link-effect');
        jQuery('.nav-holder .dd-container').css("cssText", "display: none !important;");
    } else {
        jQuery('.primary-menu nav').addClass('link-effect');
    }

});

// position mega menu correctly

jQuery.fn.t4p_position_megamenu = function (variables) {

    var reference_elem = '';
    if (jQuery('.header_v4').length) {
        reference_elem = jQuery(this).parent().parent('nav').parent();
    } else if (jQuery('.headerbar').length) {
        reference_elem = jQuery('.container-header');
    } else {
        reference_elem = jQuery(this).parent().parent('nav');
    }

    if (jQuery(this).parent().parent('nav').length) {

        var main_nav_container = reference_elem,
                main_nav_container_position = main_nav_container.offset(),
                main_nav_container_width = main_nav_container.width(),
                main_nav_container_left_edge = main_nav_container_position.left,
                main_nav_container_right_edge = main_nav_container_left_edge + main_nav_container_width;

        jQuery('.t4p-navbar-nav .t4p-megamenu-menu').mouseenter(function () {
            var li_item = jQuery(this),
                    li_item_position = li_item.position(),
                    megamenu_wrapper = li_item.find('.t4p-megamenu-wrapper'),
                    megamenu_wrapper_width = megamenu_wrapper.outerWidth(),
                    megamenu_wrapper_position = 0;

            //check if there is a megamenu
            if (megamenu_wrapper.length) {
                megamenu_wrapper.removeAttr('style');

                if (jQuery('.sticky-header').hasClass('sticky')) {
                    /* add mega-menu effect to stickyheader */
                    var main_stickynav_container_width = jQuery('.sticky-menu')[0].getBoundingClientRect().width;

                    if (megamenu_wrapper_width < main_stickynav_container_width) {

                        if (megamenu_wrapper_width < (main_stickynav_container_width - li_item_position.left)) {
                            megamenu_wrapper_position = '';
                            megamenu_wrapper.css('left', megamenu_wrapper_position);
                        } else if (megamenu_wrapper_width > (main_stickynav_container_width - li_item_position.left)) {
                            megamenu_wrapper.css('right', '0');
                        }

                    } else {
                        if (jQuery('#sticky-logo').length) {
                            var stickylogoWidth = jQuery('#sticky-logo')[0].getBoundingClientRect().width;
                            var stickylogoactualwidth = '-' + (stickylogoWidth + 15) + 'px';
                            jQuery('#header.sticky-header .t4p-megamenu-wrapper').css('left', stickylogoactualwidth);
                        } else {
                            jQuery('#header.sticky-header .t4p-megamenu-wrapper').css('left', '-15px');
                        }
                    }

                } else if (jQuery('.headerbar').length) {
                    if (megamenu_wrapper_width < main_nav_container_width) {
                        var main_halfnav_container_width = jQuery('.headerbar')[0].getBoundingClientRect().width;

                        if (megamenu_wrapper_width < (main_halfnav_container_width - li_item_position.left)) {
                            megamenu_wrapper_position = '';
                            megamenu_wrapper.css('left', megamenu_wrapper_position);
                        } else if (megamenu_wrapper_width > (main_halfnav_container_width - li_item_position.left)) {
                            megamenu_wrapper.css('right', '0');
                        }

                    } else {
                        var headerlogoWidth = jQuery('.logobar')[0].getBoundingClientRect().width;
                        var headerlogoactualWidth = '-' + (headerlogoWidth + 30) + 'px';
                        jQuery('.t4p-megamenu-wrapper').css('left', headerlogoactualWidth);
                    }
                } else {
                    if (megamenu_wrapper_width < main_nav_container_width) {

                        if (megamenu_wrapper_width < (main_nav_container_width - li_item_position.left)) {
                            megamenu_wrapper_position = '';
                            megamenu_wrapper.css('left', megamenu_wrapper_position);
                        } else if (megamenu_wrapper_width > (main_nav_container_width - li_item_position.left)) {
                            megamenu_wrapper.css('right', '0');
                        }

                    } else {
                        if (jQuery('.header_v0').length) {
                            megamenu_wrapper.css('left', '-15px');
                        } else {
                            megamenu_wrapper.css('left', '0');
                        }
                    }
                }
            }
        });
    }
};

// Activates the mega menu

if (jQuery.fn.t4p_position_megamenu) {
    jQuery('.t4p-navbar-nav').t4p_position_megamenu();
    jQuery('.t4p-navbar-nav .t4p-megamenu-menu').mouseenter(function () {
        jQuery(this).parent().t4p_position_megamenu();
    });
}

// For woocommerce edit-addresss form

jQuery(document).ready(function ($) {

    jQuery('.woo_editaddress').click(function (e) {
        e.preventDefault();

        var editaddress = $(this).attr('id');

        if (editaddress == 'editaddress_billing') {
            jQuery('.editaddress_billing').fadeIn();
            jQuery('.editaddress_shipping').hide();
        } else if (editaddress == 'editaddress_shipping') {
            jQuery('.editaddress_shipping').fadeIn();
            jQuery('.editaddress_billing').hide();
        }
    });

    jQuery('#saveaddress').click(function () {
        var formvalue = $('#formvalue').val();

        if (formvalue == 'billing') {
            jQuery('.editaddress_billing').fadeIn();
            jQuery('.editaddress_shipping').hide();
        } else if (formvalue == 'shipping') {
            jQuery('.editaddress_shipping').fadeIn();
            jQuery('.editaddress_billing').hide();
        }
    });

});
