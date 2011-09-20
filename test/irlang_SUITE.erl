-module(irlang_SUITE).

% easier than exporting by name
-compile(export_all).

% required for common_test to work
-include("ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common test callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() -> [test1, test2].

% required, but can just return Config. this is a suite level setup function.
init_per_suite(Config) ->
    % do custom per suite setup here
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(Config) ->
    % do custom per suite cleanup here
    Config.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(TestCase, Config) ->
    % do custom test case setup here
    Config.

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(TestCase, Config) ->
    % do custom test case cleanup here
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

test1(Config) ->
    % write standard erlang code to test whatever you want
    % use pattern matching to specify expected return values
    ok.

test2(Config) -> ok.


