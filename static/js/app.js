
function decodeStateFromUrl() {
    var state = {}
    window.location.search.substr(1).split("&").forEach(function (keyVal) {
        var split = keyVal.split("=")
        var key = split[0]
        var value = split[1]
        if (key !== undefined && value !== undefined) {
            state[decodeURIComponent(key)] = decodeURIComponent(value)
        }
    })
    return state
}

function encodeStateToUrl(state, push) {
    var stateForUrl = ""
    for (var key in state) {
        var value = state[key]
        if (state.hasOwnProperty(key) && value) {
            if (stateForUrl.length > 0) {
                stateForUrl += "&"
            }
            stateForUrl += encodeURIComponent(key)
            stateForUrl += "="
            stateForUrl += encodeURIComponent(value)
        }
    }
    var newUrl = window.location.href.split("?")[0]
    if (stateForUrl.length > 0) {
        newUrl += "?"
        newUrl += stateForUrl
    }
    if (newUrl !== window.location.href) {
        if (push) {
            window.history.pushState(undefined, "", newUrl)
        } else {
            window.history.replaceState(undefined, "", newUrl)
        }
    }
}

/**
 * State mechanism:
 *  - Subscribe to state changes
 *  - Sync state with URL
 *  - State changes are only propagated when the state actually changes, not on every .set()
 *  - Old state is saved in DOM so that e.g. when on back the browser loads the page with the old state,
 *    No unnecessary XHR is triggered.
 * @param {String} name: The name of the property
 * @param {Boolean} push: if true changes to this property createa new history entry in the browser history
 * @param {String} selector: jQuery selector where the current state is stored in
 */
function StateProp(name, push, selector) {
    var handlers = []
    this.name = name
    var attr = 'data-' + name

    function callHandlers(value) {
        handlers.forEach(function (handler) {
            handler(value)
        })
    }

    this.getLastValue = function () {
        return $(selector).attr(attr)
    }

    function setLastValue(val) {
        if (val === undefined) {
            $(selector).removeAttr(attr)
        } else {
            $(selector).attr(attr, val)
        }
    }

    this.subscribe = function (handler) {
        handlers.push(handler)
    }

    this.set = function (value) {
        var currentValue = this.getLastValue()
        if (currentValue !== value) {
            console.log("set", name, value, "currentValue", currentValue)
            callHandlers(value)
            var state = decodeStateFromUrl()
            state[name] = value
            encodeStateToUrl(state, push)
            setLastValue(value)
        }
    }

    var that = this;
    window.addEventListener('popstate', function () {
        that.set(decodeStateFromUrl()[name])
    });
}

/**
 * The state:
 *  - modalUrl: Url of the open modal. Undefined if closed.
 *  - searchQuery: The query in the search bar. Undefined if empty.
 */
var State = {
    modalUrl: new StateProp("modalUrl", false, "#ajax-modal"),
    searchQuery: new StateProp("query", true, "#audiobook-container")
}

/**
 * Handle modal
 */
State.modalUrl.subscribe(function (modalUrl) {
    if (modalUrl) {
        $("#ajax-modal .modal-content").load(modalUrl, function () {
            $("#ajax-modal")
                .modal({ show: true })
                .on("hidden.bs.modal", function () {
                    State.modalUrl.set(undefined)
                })

            /**
             * Initialize: Copy value from text input that contains RSS on button press.
             */
            new ClipboardJS("#copy-rss-button")
        })
    } else {
        $("#ajax-modal").modal("hide");
    }
})

/**
 * Handle search query
 */
State.searchQuery.subscribe(function (query) {
    // Uses POST => Server renders only book container
    var data = { "query": query || "" }
    $("#search input").val(query || "")
    $("#audiobook-container").load(window.location.href, data)
})

$(document).ready(function () {
    /**
     * Modals for books
     */
    $(document).on("click", "#audiobook-container:not(.selectable) .audiobook .img-wrapper[data-modal-url]", function () {
        State.modalUrl.set($(this).attr("data-modal-url"));
    })

    /**
     * Book search: On keyup update .container from same URL 300 ms debounced
     */
    var timer = null
    $("#search input").on("keyup", function () {
        if (timer !== null) {
            clearTimeout(timer)
        }
        var input = $(this)
        timer = setTimeout(function () {
            State.searchQuery.set(input.val())
        }, 300)
    })

    /**
     * Initial state from URL
     */
    var initialState = decodeStateFromUrl()
    for (var prop in State) {
        if (State.hasOwnProperty(prop)) {
            State[prop].set(initialState[State[prop].name])
        }
    }
})