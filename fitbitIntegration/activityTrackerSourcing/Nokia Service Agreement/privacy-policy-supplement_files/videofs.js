var env = env||{};
env.lang = env.lang||'en';

function onYouTubePlayerReady(playerId) {
	console.log('player ready');
}


(function() {

	$(function() {
		$(document).on('click', '.videofs-play', function(e) {
            e.preventDefault();
            e.stopPropagation();
			if ($('.video-fs').length == 0) {
				$('body').append('<div class="video-fs"></div>');
			}
			var src = $(this).attr('href');
			if (src.indexOf('?')>-1) {
				src = src.substring(0, src.indexOf('?'));
			}
			src += "?autoplay=1&start=0&loop=0&color=white&showinfo=0&enablejsapi=1&modestbranding=1&rel=0&autohide=1&iv_load_policy=3&cc_load_policy=0&controls=0&vq=hd720&hl="+env.lang+"";
			$('.video-fs').html('<iframe id="video-fs-iframe" frameborder="0" src="'+src+'"></iframe><div class="video-fs-controls"><span class="video-fs-controls-play"><div class="video_icon"></div></span></div><div class="icon-menu-close"><div class="video_icon"></div></div>').show();
			$('.video-fs').attr('class', 'video-fs animated fadeIn');
			$('.page-contents').hide();
			var keyup_evt, do_close = function() {
				if ($('.video-fs').length) {
					// $('.video-fs').attr('class', 'video-fs animated fadeOut');
					// setTimeout(function(){ $('.video-fs').remove(); }, 800);
					$('.video-fs').remove(); 
					$('.page-contents').show();
					$(document).off('keyup', keyup_evt);
				}
			}
			keyup_evt = function(e) {
				if (e.keyCode === 27) do_close();
				if (e.keyCode === 32) { // space (pause/play)
				}
				if (e.keyCode === 37) { // left (start)
				}
				if (e.keyCode === 39) { // right
				}
				if (e.keyCode >= 48 && e.keyCode <= 57) { // numpad
				}
			}
			$('.video-fs .icon-menu-close').on('click', do_close);
			$(document).on('keyup', keyup_evt);
			$(window).on('resize', function() {
				$("#video-fs-iframe")
					.width(.9*$(window).width())
					.height($("#video-fs-iframe").width()/16*9);
				if ($("#video-fs-iframe").height()>$(window).height()*.8) {
					$("#video-fs-iframe")
						.height($(window).height()*.8)
						.width($("#video-fs-iframe").height()*16/9);
				}
				$("#video-fs-iframe")
					.css('left',($(window).width()-$("#video-fs-iframe").width())/2)
					.css('top',($(window).height()-$("#video-fs-iframe").height())/2);
			}).trigger('resize');
		});

	});

}).call();
