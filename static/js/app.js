/**
 * Modals
 */
$(document).ready(function() {
    $(".ajax-modal[data-modal-url]").on("click", function() {
        $('#ajax-modal .modal-content').load($(this).attr("data-modal-url"), function() {
            $('#ajax-modal').modal({show:true});
        })
    });

});

/**
 * Update books results from search
 */
$(document).ready(function() {

});