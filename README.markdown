
## About

erlang\_twitter is a simple gen\_server based client to Twitter's API. It supports the entire Twitter API and all combinations of options including API method aggregation.

    1> twitter_client:start().
    {ok,<0.44.0>}
    2> twitter_client:add_session("ngerakines", "ugottabejoking").
    ok
    3> twitter_client:call("ngerakines", status_friends_timeline).
    [{status,"Sun Aug 10 05:41:49 +0000 2008","883056945",
     "Watching \"Bait Shop\" starring Bill Engvall and Billy Ray Cyrus.",
     "web","false",[],[],[],undefined}
     {status, ...} | ...]

The module layout is relatively simple and self explanatory. Each of the Twitter API methods map directly to a module function. For example, the Twitter API "statuses/friends\_timeline.xml" can be accessed using twitter\_client:status\_friends\_timeline/4.

Each API method function has the same function parameters. They are a string representing the root API url, the login and password for the account and then a list of API method specific arguments. API methods that do not use certain arguments ignore them.

The _status_ and _user_ records as defined in twitter\_client.hrl represent statuses and users as returned by API requests.

Ideally all API method functions are accessed through the gen\_server API exposed by the module. The twitter\_client:call/2 and twitter\_client:call/3 functions are used for this purpose. The API method functions can also be accessed directly if you wish to bypass the twitter\_client gen\_server altogether.

## Multiple Sessions

This module can be used to make API requests on behalf of multiple accounts. Use the twitter\_client:add\_session/2 function to do so.

    2> twitter_client:add_session("usera", "password").
    ok
    3> twitter_client:add_session("userb", "secret").
    ok
    4> twitter_client:call("usera", status_friends_timeline).
    ...
    5> twitter_client:call("userb", status_friends_timeline).
    ...

## Throttling

This module supports a limited way to throttle API calls. Internally, it tracks a delay value and the last time an API call was made (regardless if it was successful or not). Using the twitter\_client:delay/0 function will return the number of seconds you should wait before making the next API request.

NOTE: This functionality does not incorporate any server-side throttling. It is simply based on the last time an API call was made taking into account the delay value set. The delay value can be updated using the the twitter\_client:set/2 function

    1> twitter_client:set(delay, 2).
    ok

It is a good idea to wait at least 1 second between every API request. Not only is it polite, but it increases your application's karma and will get you laid.

## Identi.ca support

This module has gone through very light testing to verify Identi.ca support. Accessing an Identi.ca server's API can be done by setting the _base\_url_ setting for the client.

    4> twitter_client:set(base_url, "http://identi.ca/api/").
    ok

By default, the _base\_url_ is set to "http://twitter.com/".

## Packaging

Use `make package-debian` and `make install-debian` to build a .deb file for Debian deployment.

## Contributions

* Harish Mallipeddi
* Joshua Miller
