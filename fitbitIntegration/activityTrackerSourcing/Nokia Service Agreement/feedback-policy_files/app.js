(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var device = require('./utils/device.js');

window.healthapp = {
    device  : device
};
},{"./utils/device.js":8}],2:[function(require,module,exports){
new (nkhAbstractClass.extend({
    init: function() {
        this.showHideMenuHandler = this.showHideMenu.bind(this);
        this.resizeHandler = this.resize.bind(this);
    
        this._super();
        
        this.resize();
    },

    addObservers: function() {
        this._super();
        
        $('footer h6')
            .off('click', this.showHideMenuHandler)
            .on('click', this.showHideMenuHandler);
        $(window)
            .off('resize', this.resizeHandler)
            .on('resize', this.resizeHandler);
    },

    destroy: function() {
        this._super();
        
        this.showHideMenuHandler = null;
        this.resizeHandler = null;
    },

    removeObservers: function() {
        this._super();

        $('footer h6')
            .off('click', this.showHideMenuHandler);
        $(window)
            .off('resize', this.resizeHandler);
    },
    
    showHideMenu: function(evt) {
        var h6 = $(evt.currentTarget);
        if(h6.hasClass('open') == true) {
            this.hideMenu(h6);
        } else {
            this.hideMenu();
            this.showMenu(h6);
        }
    },
    
    showMenu: function(h6) {
        h6.addClass('open');
        h6.next('ul').addClass('open');
    },
    
    hideMenu: function(h6) {
        if(h6 != null) {
            h6.removeClass('open');
            h6.next('ul').removeClass('open');
            return;
        }

        $('footer h6, footer ul').removeClass('open');
    },
    
    resize: function() {
        this.hideMenu();
    }
}));
},{}],3:[function(require,module,exports){
new (nkhAbstractClass.extend({
    init: function() {
        this.showHideMenuHandler = this.showHideMenu.bind(this);
        this.showHideSubmenuHandler = this.showHideSubmenu.bind(this);
        this.resizeHandler = this.resize.bind(this);
        this.hideMenuHandler = this.hideMenu.bind(this);
        this.toggleFixedMenuHandler = this.toggleFixedMenu.bind(this);

        this.lastScrollTop = 0;
        this.fired = 0;
        this.header = $('header');
        this.headerBody = $('#headerBody');
        this.body = $('body');
        this.main = $('main');
        this.orderBar = $('.order-bar');

        this._super();

        this.resize();
    },

    addObservers: function() {
        this._super();

        $('header #headerLogo')
            .off('click', this.hideMenuHandler)
            .on('click', this.hideMenuHandler);
        $('header #headerButtonBurger, header #headerButtonClose')
            .off('click', this.showHideMenuHandler)
            .on('click', this.showHideMenuHandler);
        $('header .headerMenuItem.hasChildren > .headerMenuLink')
            .off('click', this.showHideSubmenuHandler)
            .on('click', this.showHideSubmenuHandler);
        $(window)
            .off('resize', this.resizeHandler)
            .on('resize', this.resizeHandler)
            .off('scroll', this.toggleFixedMenuHandler)
            .on('scroll', this.toggleFixedMenuHandler);
        $(document)
            .off('click', 'header .headerSubmenuClose, html:has(header.open), html:has(header .headerMenuItem.open)', this.hideMenuHandler)
            .on('click', 'header .headerSubmenuClose, html:has(header.open), html:has(header .headerMenuItem.open)', this.hideMenuHandler);
    },

    destroy: function() {
        this._super();

        this.showHideMenuHandler = null;
        this.showHideSubmenuHandler = null;
        this.resizeHandler = null;
        this.hideMenuHandler = null;
        this.toggleFixedMenuHandler = null;
    },

    removeObservers: function() {
        this._super();

        $('header #headerLogo')
            .off('click', this.hideMenuHandler);
        $('header #headerButtonBurger, header #headerButtonClose')
            .off('click', this.showHideMenuHandler);
        $('header .headerMenuItem.hasChildren > .headerMenuLink')
            .off('click', this.showHideSubmenuHandler);
        $(window)
            .off('resize', this.resizeHandler)
            .off('scroll', this.toggleFixedMenuHandler);
        $(document)
            .off('click', 'header .headerSubmenuClose, html:has(header.open), html:has(header .headerMenuItem.open)', this.hideMenuHandler);
    },

    showHideMenu: function() {
        if(this.header.hasClass('open') == true) {
            this.hideMenu();
        } else {
            this.showMenu();
        }
    },

    showMenu: function() {
        this.header.addClass('open');
        $(document).trigger('nkh::menuChanged');
    },

    toggleFixedMenu: function() {
      // This function check we scroll up or down and show the navigation on scroll up
      if(!(window.healthapp.device.isMobile()) && $(window).width() >= 768){

        var sTposition = $(window).scrollTop();

        if (!$('#headerMenu ul li').hasClass('open')) {
          // Sroll UP
          if(sTposition > this.headerBody.height() * 3 && sTposition < this.lastScrollTop && this.fired == 0){
            this.header.removeClass('slideOutUpTop');
            this.header.addClass('fixedMenu slideInDownTop');

            // If we are on product page
            if(this.body.hasClass('body-store')){
              if($('#sub-header').hasClass('close')){
                $(document).trigger('nkh::menuIsUp');
              } else if($('#sub-header').hasClass('open') && sTposition > $('#sub-header').height()){
                $(document).trigger('nkh::menuIsUp');
              }
            }

            // pass once time in scroll function
            this.fired = 1;

          } else if(sTposition > this.headerBody.height() * 3 && sTposition > this.lastScrollTop && this.header.hasClass('slideInDownTop')){
            // Scroll DOWN
            this.header.removeClass('slideInDownTop');
            this.header.addClass('slideOutUpTop');

            // If we are on product page
            if(this.body.hasClass('body-store')){
              $(document).trigger('nkh::menuIsDown');
            }

            this.fired = 0;


          } else if(sTposition < this.lastScrollTop && $('#sub-header').hasClass('open')){
            // Global nav is open
            if(sTposition <= $('#sub-header').height()){
              $('.order-bar').removeAttr('style');
              this.header.css({
                  'position': 'absolute',
                  'top': 'initial'
              });
              if(this.body.hasClass('body-store')){
                this.orderBar.removeClass('order_slideInDownTop');
              }
            }

            this.fired = 0;

          } else if(sTposition > this.lastScrollTop && $('#sub-header').hasClass('open')){
            if(sTposition >= $('#sub-header').height()){
              $('header').removeAttr('style');
            }
          } else if(sTposition <= this.header.height()) {
            this.header.removeAttr('class');
            this.orderBar.removeClass('order_slideInDownTop');

            //Elastic effect on orderbar
            if(!$('#headerMenu ul li').hasClass('open')){
              this.orderBar.addClass('order-bar-animated');
              setTimeout(function(){
                $('.order-bar').removeClass('order-bar-animated');
              }, 400);
            }

            this.fired = 0;

          }
        }

        this.lastScrollTop = sTposition;


      }
    },

    hideMenu: function(evt) {
        if(evt != null) {
            var targetElement = $(evt.target);
            if(
                targetElement.hasClass('headerSubmenuClose') == false &&
                (
                    targetElement.closest('.headerMenuItem').length > 0 ||
                    targetElement.closest('#headerButtonBurger').length > 0 ||
                    targetElement.closest('#headerButtonClose').length > 0
                )
            ) {
                return;
            }
        }
        this.hideSubmenu();
        this.header.removeClass('open');
        $(document).trigger('nkh::menuChanged');
    },

    showHideSubmenu: function(evt) {
        headerMenuItem = $(evt.currentTarget).closest('.headerMenuItem');
        var openMenu = !headerMenuItem.hasClass('open');
        this.hideSubmenu();
        if(openMenu == true) {
            headerMenuItem.addClass('open');
            $(document).trigger('nkh::menuChanged');
        }
    },

    hideSubmenu: function() {
        $('header .headerMenuItem.hasChildren').removeClass('open');
        this.main.css('margin-top', '0');
        $(document).trigger('nkh::menuChanged');
    },

    resize: function() {
        //this.hideMenu();
    }
}));

},{}],4:[function(require,module,exports){
$(document).ready(function() {
  if (document.location.href.indexOf('btmsg') > -1 ) {
    // Show popIn
    $('._popIn_nokia-withings._btmsggeneral').show();
  }
  if (document.location.href.indexOf('btdbmsg') > -1 ) {
    // Show popIn
    $('._popIn_nokia-withings._btmsghome').show();
  }
  // Hide POP UP PROMO
  if ($('.header-banner').length > 0 ){
    $('.header-banner .btn-close-banner').on('click touch', function() {
      // Important : add domain option to be manipulate by backend response
      $.cookie('_bh',$.cookie('_b'),{path:'/', domain: env.site_cookie_domain});
      $('.header-banner').remove();
    });
  }
});

},{}],5:[function(require,module,exports){
new (nkhAbstractClass.extend({
    init: function() {
        this.showMenuHandler = this.showMenu.bind(this);
        this.hideMenuHandler = this.hideMenu.bind(this);
        this.showHideSubmenuHandler = this.showHideSubmenu.bind(this);

        this._super();
    },

    addObservers: function() {
        this._super();

        $('header #headerLogo')
            .off('click', this.showMenuHandler)
            .on('click', this.showMenuHandler);
        $('#sub-header #sub-header_body_sites_title')
            .off('click', this.showHideSubmenuHandler)
            .on('click', this.showHideSubmenuHandler);
        $(document)
            .off('click', '#sub-header #sub-header_body_title_close, html:has(#sub-header.open)', this.hideMenuHandler)
            .on('click', '#sub-header #sub-header_body_title_close, html:has(#sub-header.open)', this.hideMenuHandler);
    },

    destroy: function() {
        this._super();

        this.showMenuHandler = null;
        this.hideMenuHandler = null;
        this.showHideSubmenuHandler = null;
    },

    removeObservers: function() {
        this._super();

        $('header #headerLogo')
            .off('click', this.showMenuHandler);
        $('#sub-header #sub-header_body_sites_title')
            .off('click', this.showHideSubmenuHandler);
        $(document)
            .off('click', '#sub-header #sub-header_body_title_close, html:has(#sub-header.open)', this.hideMenuHandler);
    },

    showMenu: function(evt) {
        if($(evt.target).attr('id') != 'headerLogoHealth') {

          if($(window).scrollTop() > 1.5 * $('#sub-header').height()){
            $('html,body').animate({
                'scrollTop': 0
            }, 500);
          };

          $('header').removeAttr('class');
          $('header').removeAttr('style');

          if($(window).scrollTop() <= 1.5 * $('#sub-header').height()){
            $('#sub-header').toggleClass('open close');
          } else if($(window).scrollTop() > 1.5 * $('#sub-header').height() && $('#sub-header').hasClass('close')){
            $('#sub-header').toggleClass('open close');
          }
          $('.order-bar').removeAttr('style');
          $('header').removeAttr('style');
          $('header').removeAttr('class');
          $(document).trigger('nkh::menuChanged');

          this.stopEvent(evt);
          return false;
        }
    },

    hideMenu: function(evt) {
        if($('header').hasClass('fixedMenu')){
          $('header').removeClass('fixedMenu');
        }
        if(evt != null) {
            var targetElement = $(evt.target);
            if(
                targetElement.attr('id') != 'sub-header_body_title_close' &&
                targetElement.closest('#sub-header').length > 0
            ) {
                return;
            }
        }

        this.hideSubmenu();
        $('#sub-header').removeClass('open');
        $('#sub-header').addClass('close');
        $('header').removeAttr('style');
        $(document).trigger('nkh::menuChanged');
    },

    showHideSubmenu: function(evt) {
        $('#sub-header_body_sites_title').toggleClass('open');
        $('#sub-header_body_sites_links').toggleClass('open');
        $(document).trigger('nkh::menuChanged');
    },

    hideSubmenu: function() {
        $('#sub-header_body_sites_title').removeClass('open');
        $('#sub-header_body_sites_links').removeClass('open');
        $(document).trigger('nkh::menuChanged');
    }
}));

},{}],6:[function(require,module,exports){
var wiKmi = {

	// Configuration
	selectorBtn  : '.btn-wikmi',
	selectorPanel: '.register-wipanel',
	selectorScrollOpen: '.kmi-to-open',
	cookieName : 'flag-kmi',
	data: {},

	// Initialisation
	init: function(options) {
		var main = this, options = options || {};

		if ($(this.selectorBtn).length === 0 || $(this.selectorPanel).length === 0 ) {
			return;
		}

		// Attach click event on the kmi event
		$(this.selectorBtn).unbind('click').on('click', function(e) {
			e.preventDefault();
			main.openPanel(e, this);
			return false;
		});

		// Attach event submit on the active form
		$("form.active", this.selectorPanel).on("submit", function(e){main.submitEmail(e, this);});

		// Change cookie kmi
		var topicEle = $("form.active [data-topic]", this.selectorPanel);
		if (topicEle.length && topicEle.data('topic')) {
			this.cookieName = topicEle.data('topic') + '-kmi';
		}

		// Attach event close button
		$(".btn-close, .close-link", this.selectorPanel).click(function(){
			$(main.selectorPanel).hide();
		});

		// Attach event next button
		$(".btn-next", this.selectorPanel).click(function(e){
			main.next();
		});

		// Open popup on the scroll
		if ($(main.selectorScrollOpen).length && $.cookie(this.cookieName) === undefined) {
			var waypoint = new Waypoint({
				element: $(main.selectorScrollOpen)[0],
				handler: function(direction) {
					// Check if the cookie has not been created after the module init
					if (direction == 'down' && $.cookie(main.cookieName) === undefined) {
						main.openPanel();
					}
				}
			});
		}

		// Check the hashtag to open popup
		if (window.location.hash == '#kmi') {
			$(this.selectorBtn).trigger('click');
		}
	},

	openPanel: function(){
		$(this.selectorPanel).show();
		$.cookie(this.cookieName, 1);
	},

	// Open the panel
	openPanelEvent: function(e, btn) {
		e.preventDefault();
		this.openPanel();
	},

	// Submit the email and display message
	submitEmail: function(e, form) {
		e.preventDefault();

		var main = this,
			re = /^.+@.+\..+$/i,
		    url = env.baseroute + "/services/email",
			currentForm = $(form),
			submitBtn = currentForm.find('button[type=submit]'),
			email = $("#"+submitBtn.data('input')).val();

		var data = {
			topic: submitBtn.data('topic'),
			email: email
		};

		if(!re.test(email.trim())) {
			$("#alert_"+submitBtn.data('input')).css("display","block");
			return false;
		} else {
			submitBtn.attr('disabled','disabled');
			$.ajax({
				type: "POST",
				url: url,
				data: data,
				success: function(r) {
					var response = JSON.parse(r);
					main.data.email = data.email;
					main.data.hash = response.hash;
					main.data.id = response.id;
					submitBtn.removeAttr('disabled');
					main.next();
				},
				error: function(r) {
					alert('An error occured, please try later');
					submitBtn.removeAttr('disabled');
				}
			});
		}
	},

	// Submit next form
	submitComplement: function(e, form) {
		e.preventDefault();
		if (this.data.hash && this.data.id) {
			var main = this,
			    url = env.baseroute + "/services/email/" + main.data.id,
				currentForm = $(form),
				submitBtn = currentForm.find('button[type=submit]'),
				formDataArray = currentForm.serializeArray(),
				email = main.data.email;

			submitBtn.attr('disabled','disabled');

			// Init data request
			var data = {
				email : this.data.email,
				hash : this.data.hash
			};

			// Complete data with the form
			formDataArray.forEach(function(formData){
				data[formData.name] = formData.value;
			});

			$.ajax({
				type: "POST",
				url: url,
				data: data,
				success: function(r) {
					submitBtn.removeAttr('disabled');
					main.next();
				},
				error: function(r) {
					alert('An error occured, please try later');
					submitBtn.removeAttr('disabled');
				}
			});
		} else {
			// Hash error
		}
	},

	// Check and display next form or thanks message
	next: function() {
		var main = this,
			activeForm = $("form.active", this.selectorPanel).addClass('passed'),
			nextForm = $("form:not(.passed)", this.selectorPanel).first();

		if (main.data.hash && main.data.id && nextForm.length > 0) {
			activeForm.removeClass('active').parent('article').fadeOut(function(){
				nextForm.addClass('active').parent('article').fadeIn();
				nextForm.submit(function(e){
					main.submitComplement(e, this);
				});
			});

		} else {
			activeForm.parent('article').fadeOut(function(){
				$('.wikmi-back', this.selectorPanel).fadeIn();
			});

		}
	}
};
$(function(){
    wiKmi.init();
});

},{}],7:[function(require,module,exports){
new (nkhAbstractClass.extend({
    initVariable: function() {
        this._super();

        this._status = 'docked';
    },

    init: function() {
        this.lastScrollTop = 0;
        this.resizeHandler = this.resize.bind(this);
        this.scrollHandler = this.scroll.bind(this);
        this.scrollToBuyButtonHandler = this.scrollToBuyButton.bind(this);
        this.refreshHandler = this.refresh.bind(this);
        this.toggleOrderUpHandler = this.toggleOrderUp.bind(this);
        this.toggleOrderDownHandler = this.toggleOrderDown.bind(this);

        this._super();

        this.refresh();
    },

    addObservers: function() {
        this._super();

        $('.shop-anchor')
            .off('click touchend', this.scrollToBuyButtonHandler)
            .on('click touchend', this.scrollToBuyButtonHandler);

        $(document)
            .off('nkh::menuChanged', this.refreshHandler)
            .on('nkh::menuChanged', this.refreshHandler)
            .off('nkh::menuIsUp', this.toggleOrderUpHandler)
            .on('nkh::menuIsUp', this.toggleOrderUpHandler)
            .off('nkh::menuIsDown', this.toggleOrderDownHandler)
            .on('nkh::menuIsDown', this.toggleOrderDownHandler);

        $(window)
            .off('resize', this.resizeHandler)
            .on('resize', this.resizeHandler)
            .off('scroll', this.scrollHandler)
            .on('scroll', this.scrollHandler);
    },

    destroy: function() {
        this._super();

        this.resizeHandler = null;
        this.scrollHandler = null;
        this.scrollToBuyButtonHandler = null;
        this.refreshHandler = null;
    },

    removeObservers: function() {
        this._super();

        $('.shop-anchor')
            .off('click touchend', this.scrollToBuyButtonHandler);

        $(document)
            .off('nkh::menuChanged', this.refreshHandler)
            .off('nkh::menuIsUp', this.toggleOrderUpHandler)
            .off('nkh::menuIsDown', this.toggleOrderDownHandler);

        $(window)
            .off('resize', this.resizeHandler)
            .off('scroll', this.scrollHandler);
    },

    resize: function() {
        this.refresh();
    },

    scroll: function() {
        this.refresh();
    },

    refresh: function() {
        this.setOrderBarPosition();
        this.hideBuyButton();
    },

    toggleOrderUp: function() {
      // Navigation is up
      var orderBar = $('.order-bar');
      orderBar.removeClass('order_slideOutUpTop');
      orderBar.css({
        'position': 'fixed',
        'top': $('header').height()
      });
      orderBar.addClass('order_slideInDownTop');
    },

    toggleOrderDown: function() {
      // Navigation is down
      var orderBar = $('.order-bar');
      orderBar.removeClass('order_slideInDownTop');
      orderBar.css({
          'position': 'fixed',
          'top': 0
      });
      orderBar.addClass('order_slideOutUpTop');
    },

    setOrderBarPosition: function() {
        var orderBar = $('.order-bar');

        // Header
        if($('#headerMenu ul').outerHeight() > 0) {
            // Mobile
            var isMobile = true;
            var headerMenuHeight = $('#headerMenu').outerHeight() || 0;
        } else {
            // Desktop
            var isMobile = false;
            var headerMenuHeight = $('.headerMenuItem.open .headerSubmenu').outerHeight() || 0;
        }

        // Sub header
        var subHeaderHeight = $('#sub-header.open').outerHeight() || 0;
        if(subHeaderHeight > 0) {
            var threshold = orderBar.outerHeight() + subHeaderHeight;
        } else {
            var threshold = orderBar.outerHeight() + headerMenuHeight;
        }
        var scrollValue = $(window).scrollTop();

        if(this._status == 'docked') {
            if(threshold < scrollValue) {
              // Start
              this._status = 'moving';

              orderBar.css({
                  'position': 'fixed',
                  'top': 0
              });

            } else {
              orderBar.css({
                'top': (isMobile == true ? 0 : headerMenuHeight)
              });
            }
        } else {
            if(threshold >= scrollValue) {
                // Stop
                this._status = 'docked';

                orderBar.css({
                    'position': 'absolute',
                    'top': (isMobile == true ? 0 : headerMenuHeight)
                });
            }
        }
    },

    hideBuyButton: function() {
        if(
            window.healthapp != null &&
            window.healthapp.device.isMobile() != null &&
            $('.shop-form').length >= 1
        ) {
            var orderBarMobileContainer = $('.order-bar-mobile-container');
            $('.product-shop_addtocart').waypoint({
                'handler': function(direction) {
                    if(direction === 'down') {
                        orderBarMobileContainer
                            .removeClass('slideInUp')
                            .addClass('slideOutDown');
                    } else if(direction === 'up') {
                        orderBarMobileContainer
                            .removeClass('slideOutDown')
                            .addClass('slideInUp');
                    }
                },
                'offset': 'bottom-in-view'
            });

            $('.product-shop_addtocart').waypoint({
                'handler': function(direction) {
                    if(direction === 'down') {
                        orderBarMobileContainer
                            .removeClass('slideOutDown')
                            .addClass('slideInUp');
                    } else if(direction === 'up') {
                        orderBarMobileContainer
                            .removeClass('slideInUp')
                            .addClass('slideOutDown');
                    }
                },
                'offset': '-' + $('.order-bar').height()
            });
        }
    },

    scrollToBuyButton: function() {
        $('html,body').animate({
            'scrollTop': $('#shop-title').offset().top - $('.order-bar-container').height()
        }, 1000);
    }
}));

},{}],8:[function(require,module,exports){
var device = {

    isAndroid: function() {
        return navigator.userAgent.match(/Android/i);
    },

    isBlackBerry: function() {
        return navigator.userAgent.match(/BlackBerry/i);
    },

    isiOS: function() {
        return navigator.userAgent.match(/iPhone|iPad|iPod/i);
    },

    isOpera: function() {
        return navigator.userAgent.match(/Opera Mini/i);
    },

    isWindows: function() {
        return navigator.userAgent.match(/IEMobile/i);
    },

    isMobile: function () {
        return (this.isAndroid() || this.isBlackBerry() || this.isiOS() || this.isOpera() || this.isWindows());
    }
}

module.exports = device;
},{}]},{},[5,3,2,4,7,6,1]);
