# <%= (getf env :name) %>

<% @if description %><% @var description %><% @endif %>

## Usage

## Installation
<% @if author %>
## Author

* <% @var author %><% @if email %> (<% @var email %>)<% @endif %>

## Copyright

Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> <% @var author %><% @if email %> (<% @var email %>)<% @endif %>
<% @endif %><% @if license %>
# License

Licensed under the <% @var license %> License.
<% @endif %>
