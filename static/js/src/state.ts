export function decodeStateFromUrl(): Map<string, string> {
    let state = new Map<string, string>()
    window.location.search.substr(1).split("&").forEach((keyVal: string) => {
        let [k, v] = keyVal.split("=")
        state.set(k, v)
    })
    return state
}

function encodeStateToUrl(state: Map<string, string>, push: boolean): void {
    let stateForUrl = ""
    state.forEach((k: string, v: string) => {
        if (stateForUrl.length > 0) {
            stateForUrl += "&"
        }
        stateForUrl += `${k}=${v}`
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
            const fromUrl = decodeStateFromUrl().get(name)
            that.value = (fromUrl && decodeURIComponent(fromUrl)) || null
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
            newValue && urlState.set(this.name, newValue.toString())
            encodeStateToUrl(urlState, this.push)
        }
    }

    subscribe(handler: (value: string | null) => void): void {
        this.handlers.push(handler)
    }
}