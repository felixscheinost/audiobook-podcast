export function decodeStateFromUrl(): Map<string, string> {
    let state = new Map<string, string>()
    window.location.search.substr(1).split("&").forEach((keyVal: string) => {
        let [k, v] = keyVal.split("=")
        if (k && v) {
            state.set(decodeURIComponent(k), decodeURIComponent(v))
        }
    })
    return state
}

function encodeStateToUrl(state: Map<string, string>, push: boolean): void {
    let stateForUrl = ""
    console.log("encode", state)
    state.forEach((v: string, k: string) => {
        if (stateForUrl.length > 0) {
            stateForUrl += "&"
        }
        if (k && v) {
            stateForUrl += `${encodeURIComponent(k)}=${encodeURIComponent(v)}`
        }
    })
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
 * @param {String} name: The name of the property
 * @param {Boolean} push: if true changes to this property createa new history entry in the browser history
 * @param {String} selector: jQuery selector where the current state is stored in
 * @param {Boolean} includeInUrl: Whether to include this state in the URL
 */
export class StateProp {

    handlers: Array<(value: string | null) => void>
    private _value: string | null

    constructor(readonly name: string, readonly push: boolean, readonly selector: string, readonly includeInUrl: boolean) {
        this.handlers = []
        this._value = null
        let that = this;
        function fromUrl() {
            console.log("fromURL")
            that.value = decodeStateFromUrl().get(name) || null
        }
        window.addEventListener('popstate', fromUrl)
        fromUrl()
    }

    get value(): string | null {
        return this._value
    }

    set value(newValue: string | null) {
        if (newValue !== this._value) {
            this._value = newValue
            this.handlers.forEach(h => h(newValue))
            const urlState = decodeStateFromUrl()
            if (newValue) {
                urlState.set(this.name, newValue)
            } else {
                urlState.delete(this.name)
            }
            encodeStateToUrl(urlState, this.push)
        }
    }

    subscribe(handler: (value: string | null) => void): void {
        this.handlers.push(handler)
        handler(this.value)
    }
}