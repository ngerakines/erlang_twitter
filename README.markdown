
## About

erlang\_twitter is a client library to the Twitter API. Using it is simple:

	1> inets:start().
	...
	2> Auth = {"ngerakines", "secretpassword!"}.
	3> twitter_client:status_mentions(Auth, []).
	twitter_client:status_mentions({"ngerakines", "secretpassword"}, []).
	[{status,"Mon Nov 16 13:07:54 +0000 2009","5764367829",
	         "@ngerakines Have a safe trip back. Great seeing you & meeting @jacobvorreuter",
	         "web","false","5763249258","10590","false",
	         {user,"15592821","Francesco Cesarini","FrancescoC",
	...

The module layout is relatively simple and self explanatory. Each of the Twitter API methods map directly to a module function. For example, the Twitter API "statuses/friends\_timeline.xml" can be accessed using twitter\_client:status\_friends\_timeline/4.

Each API method function has the same function parameters. They are a string representing the root API url, the login and password for the account and then a list of API method specific arguments. API methods that do not use certain arguments ignore them.

The _status_ and _user_ records as defined in twitter\_client.hrl represent statuses and users as returned by API requests.

## TODO

 * Add support for search.
 * Add support for trends.
 * Add support for lists.
 * Document existing OAuth support.
 * Add support for the streaming API.

## Contributions

* Harish Mallipeddi
* Joshua Miller
