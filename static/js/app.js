$(document).ready(function() {
    /**
     * Modals for books
     */
    $(document).on("click", "#audiobook-container:not(.selectable) .audiobook img[data-modal-url]", function() {
        $('#ajax-modal .modal-content').load($(this).attr("data-modal-url"), function() {
            $('#ajax-modal').modal({show:true});
        });
    });

    /**
     * Select books
     */
    $("#select").on("click", function() {
        let isActive = !$(this).hasClass("active");
        $(this).text($(this).data(isActive ? "textActive" : "textInactive"));
        $(this).toggleClass("active", isActive);
        $("#audiobook-container").toggleClass("selectable", isActive);
        if (!isActive) {
            $(".audiobook .img-wrapper").removeClass("selected");
            $("#audiobook-container").trigger("audiobooks:selection:changed");
        }
    });
    $(document).on("click", ".selectable .audiobook .img-wrapper", function(){
        $(this).toggleClass("selected");
        $("#audiobook-container").trigger("audiobooks:selection:changed");
    });

    /**
     * Convert books button
     */
    $("#audiobook-container").on("audiobooks:selection:changed", function() {
        $("#convert").toggleClass("disabled", $(".img-wrapper.selected").length == 0);
        let selectedIds = $(".img-wrapper.selected").toArray().map(function(el){
            return $(el).data("bookId");
        });
        $("#convertIds").attr("value", JSON.stringify(selectedIds));
    });
})