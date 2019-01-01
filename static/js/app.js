$(document).ready(function () {
    /**
     * Modals for books
     */
    $(document).on("click", "#audiobook-container:not(.selectable) .audiobook img[data-modal-url]", function () {
        $('#ajax-modal .modal-content').load($(this).attr("data-modal-url"), function () {
            $('#ajax-modal').modal({ show: true });

            /**
             * Copy value from text input that contains RSS on button press
             */
            new ClipboardJS('#copy-rss-button');
        });
    });

    /**
     * Book search: On keyup update .container from same URL 300 ms debounced
     */
    var timer = null;
    $("#search input").on("keyup", function () {
        if (timer !== null) {
            clearTimeout(timer);
        }
        var input = $(this);
        timer = setTimeout(function () {
            // Uses POST => Server renders only book container
            var data = { 'query': input.val() };
            $('#audiobook-container').load(window.location.href, data);
        }, 300);
    });
});