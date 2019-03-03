import 'es6-shim'
import 'bootstrap'
import { StateProp, decodeStateFromUrl } from './state'
import * as $ from 'jquery'
import * as ClipboardJS from "clipboard"

/**
 * The state:
 *  - modalUrl: Url of the open modal. Undefined if closed.
 *  - searchQuery: The query in the search bar. Undefined if empty.
 */
var State = {
    modalUrl: new StateProp("modalUrl", false, "#ajax-modal", true),
    // seriesSelectedBooks: new StateProp('seriesSelectedBooks', false, "[data-rel=series-book-selection]", false)
}

/**
 * Handle modal
 */
State.modalUrl.subscribe((modalUrl: string | null) => {
    if (modalUrl !== null) {
        $("#ajax-modal").load(modalUrl, function () {
            $("#ajax-modal .modal")
                .modal({ show: true })
                .on("hidden.bs.modal", function () {
                    State.modalUrl.value = null
                })

            /**
             * Initialize: Copy value from text input that contains RSS on button press.
             */
            new ClipboardJS("#copy-rss-button")
        })
    } else {
        $("#ajax-modal .modal").modal("hide");
    }
})

$(document).ready(function () {
    /**
     * Modals for books
     */
    $(document).on("click", ".audiobook-wrapper[data-modal-url] .img-wrapper, .audiobook-wrapper[data-modal-url] .text-wrapper a[rel=title]", function (e) {
        State.modalUrl.value = $(this).parents(".audiobook-wrapper").attr("data-modal-url") || null
        e.preventDefault();
    })

    /**
     * SeriesViewR: Highlight row checkbox
     */
    $(document).on("change", "[data-rel=series-book-selection] :checkbox", function (e) {
        // var current = State.seriesSelectedBooks.getLastValue() || {}
        console.log("checkbox")
        $(this).closest("tr").toggleClass("table-primary", this.checked)
    })
    $(document).on("click", "[data-rel=series-book-selection] tr", function (e) {
        console.log("click")
        var c = $(this).find(":checkbox").get()[0]
        c.checked = !c.checked
    })
})