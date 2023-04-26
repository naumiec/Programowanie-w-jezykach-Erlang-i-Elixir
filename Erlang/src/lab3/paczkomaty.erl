%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2023 15:52
%%%-------------------------------------------------------------------
-module(paczkomaty).
-author("adamnaumiec").

%% API
-export([test/0]).



% list of random X, Y locations of lockers
getRandomLockers(Length) ->
  Lockers = [{random:uniform(10000), random:uniform(10000)} || _ <- lists:seq(1, Length)].


% list of random X, Y locations of people
getRandomPeople(Length) ->
  People = [{random:uniform(10000), random:uniform(10000)} || _ <- lists:seq(1, Length)].


% euclidean dist
dist({X1, Y1}, {X2, Y2}) -> math:sqrt( math:pow((X1-X2),2) + math:pow((Y1-Y2),2) ).


% performance test
test() ->
  Lockers = getRandomLockers(1000),
  People = getRandomPeople(1000),
  %timer:tc(fun() -> findMinDistance(People, Lockers) end),
  timer:tc(fun() -> findMinDistance(People, Lockers, self()) end).


% ---------- ---------- ---------- ---------- ----------
% find min distance between people and lockers - {distance, {PeopleLocalization, LockerLocalization}}


% sequential
findMinDistance(PeopleLocations, LockerLocations) ->
  lists:min(
    [
      [
    {dist(PeopleLocation, LockerLocation), {PeopleLocation, LockerLocation}}
    || LockerLocation <- LockerLocations ]
      || PeopleLocation <- PeopleLocations]).


% parallel
findMinDistance(PeopleLocations, LockerLocations, ParentPID) ->
  PIDs = [spawn(fun() -> findMinDistance(PeopleLocations, LockerLocations) end) || _ <- lists:seq(1, length(PeopleLocations))],
  MinDistances = [receive MinDistance -> MinDistance end || PID <- PIDs],
  MinDistance = lists:min(MinDistances),
  ParentPID ! MinDistance.


