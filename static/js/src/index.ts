import 'es6-shim'
import 'bootstrap'
import { StateProp, decodeStateFromUrl } from './state'
import * as $ from 'jquery'
import * as ClipboardJS from "clipboard"

/**
 * The state:
 *  - modalUrl: Url of the open modal. Undefined if closed.
 */
var State = {
    modalUrl: new StateProp("modalUrl", true, "#ajax-modal", true),
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
})