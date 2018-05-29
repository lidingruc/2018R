(function ($, undefined){
$(document).ready(function() {
    $('input#edit-search-block-form--2').addClass("idleField");
	$('input#edit-search-block-form--2').val('Search QIPSR');
	$('input#edit-search-block-form--2').focus(function() {
		$(this).removeClass("idleField").addClass("focusField");
        if (this.value == 'Search QIPSR'){
        	this.value = '';
    	}
        if(this.value != 'Search QIPSR'){
	    	this.select();
        }
    });
    $('input#edit-search-block-form--2').blur(function() {
    	$(this).removeClass("focusField").addClass("idleField");
        if ($.trim(this.value) == ''){
        	this.value = 'Search QIPSR';
    	}
    });
});
}(jQuery));