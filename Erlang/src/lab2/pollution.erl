%%%-------------------------------------------------------------------
%%% @author adamnaumiec
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2023 20:33
%%%-------------------------------------------------------------------
-module(pollution).
-author("adamnaumiec").

%% API
-export([create_monitor/0,
  add_station/3,
  add_value/5,
  remove_value/4,
  get_one_value/4,
  get_station_mean/3,
  get_daily_mean/3,
  get_maximum_gradient_stations/1]).



-record(measurement, {type, datetime, value}).
-record(station, {name, coordinates, measurements}).
-record(monitor, {stations, coordinates}).


create_monitor() ->
  #monitor{
    stations = dict:new(),
    coordinates = dict:new()
  }.


add_station(StationName, Coords, Monitor) when is_tuple(Coords) ->
  case station_exists(StationName, Monitor) orelse station_exists(Coords, Monitor) of
    true ->
      {error, "Station already exists"};
    false ->
      NewStations = dict:append(StationName, #station{name = StationName, coordinates = Coords, measurements = dict:new()}, Monitor#monitor.stations),
      NewCoords = dict:append(Coords, StationName, Monitor#monitor.coordinates),
      Monitor#monitor{stations = NewStations, coordinates = NewCoords}
  end.


station_exists(Coords, Monitor) when is_tuple(Coords) ->
  dict:is_key(Coords, Monitor#monitor.coordinates);


station_exists(StationName, Monitor) ->
  dict:is_key(StationName, Monitor#monitor.stations).


add_value(Station, Date, Type, Value, Monitor) ->
  case station_exists(Station, Monitor) of
    false -> {error, "Given station does not exist"};
    true ->
      CurrentStation = get_station(Station, Monitor),

      case dict:is_key({Date, Type}, CurrentStation#station.measurements) of
        true -> {error, "Measuerement already exists"};
        false ->
          Measurement = #measurement{type = Type, datetime = Date, value = Value},
          #monitor{
            stations = dict:update(
              CurrentStation#station.name,
              fun([Old | _]) ->
                [Old#station{measurements = dict:append({Date, Type}, Measurement, Old#station.measurements)}]
              end,
              Monitor#monitor.stations
            ),
            coordinates = Monitor#monitor.coordinates
          }
      end
  end.


get_station(Coords, Monitor) when is_tuple(Coords) ->
  case dict:find(Coords, Monitor#monitor.coordinates) of
    error -> {error, "Station does not exist"};
    {ok, [Name | _]} -> get_station(Name, Monitor)
  end;


get_station(StationName, Monitor) ->
  case dict:find(StationName, Monitor#monitor.stations) of
    error -> {error, "Station does not exist"};
    {ok, [Station | _]} -> Station
  end.


station_measurement_exists(Station, Type, Date) ->
  dict:is_key(Type, Station#station.measurements) andalso
    dict:is_key(Date, dict:fetch(Type, Station#station.measurements)).


remove_value(Station, Date, Type, Monitor) ->
  case station_exists(Station, Monitor) of
    false -> {error, "Station does not exist"};
    true ->
      OldStation = get_station(Station, Monitor),
      case dict:is_key({Date, Type}, OldStation#station.measurements) of
        false -> {error, "Measuerement does not exist"};
        true ->
          #monitor{
            stations = dict:update(
              OldStation#station.name,
              fun([Old | _]) ->
                [Old#station{measurements = dict:erase({Date, Type}, Old#station.measurements)}]
              end,
              Monitor#monitor.stations
            ),
            coordinates = Monitor#monitor.coordinates
          }
      end
  end.


get_one_value(Station, Date, Type, Monitor) ->
  case station_exists(Station, Monitor) of
    false -> {error, "Station doest not exist"};
    true ->
      CurrentStation = get_station(Station, Monitor),
      case dict:find({Date, Type}, CurrentStation#station.measurements) of
        error -> {error, "Measurement does not exist"};
        {ok, [Measurement | _]} ->
          Measurement#measurement.value
      end
  end.


get_station_mean(Station, Type, Monitor) ->
  case station_exists(Station, Monitor) of
    false -> {error, "Station does not exist"};
    true ->
      CurrentStation = get_station(Station, Monitor),
      Measurements = dict:filter(
        fun({D, T}, Value) -> T == Type end,
        CurrentStation#station.measurements
      ),
      case dict:size(Measurements) of
        0 -> {error, "No measurements with given type"};
        _ ->
          ValuesSum = dict:fold(
            fun(Key, [Measurement | _], AccIn) -> AccIn + Measurement#measurement.value end,
            0,
            Measurements
          ),
          ValuesSum / dict:size(Measurements)
      end
  end.


get_daily_mean(Date, Type, Monitor) ->
  StationMeans = dict:fold(
    fun(StationName, [Station | _], AccIn) ->
      [{StationName, get_station_mean(StationName, Type, Monitor)} | AccIn]
    end,
    [],
    Monitor#monitor.stations
  ),
  Measurements = [{S, get_one_value(S, Date, Type, Monitor)} || {S, _} <- StationMeans, get_one_value(S, Date, Type, Monitor) =/= {error, "Measurement does not exist"}],
  case length(Measurements) of
    0 -> {error, "No measurements with given type and date"};
    _ -> lists:sum(Measurements) / length(Measurements)
  end.


get_maximum_gradient_stations(Monitor) ->
  Stations = dict:fold(
    fun(StationName, [Station | _], AccIn) ->
      [Station | AccIn]
    end,
    [],
    Monitor#monitor.stations
  ),
  StationsWithGradients = lists:map(
    fun(Station) ->
      {Station, get_station_gradient(Station)}
    end,
    Stations
  ),
  lists:sort(
    fun({_, Gradient1}, {_, Gradient2}) ->
      Gradient1 > Gradient2
    end,
    StationsWithGradients
  ).


get_station_gradient(Station) ->
  Measurements = dict:filter(
    fun({{_, _, _}, Type}, Value) ->
      Type == "temperature"
    end,
    Station#station.measurements
  ),
  MeasurementsList = dict:fold(
    fun(Key, [Measurement | _], AccIn) ->
      [Measurement | AccIn]
    end,
    [],
    Measurements
  ),
  MeasurementsListSorted = lists:sort(
    fun(Measurement1, Measurement2) ->
      Measurement1#measurement.datetime < Measurement2#measurement.datetime
    end,
    MeasurementsList
  ),
  get_gradient(MeasurementsListSorted).


get_gradient([Measurement1, Measurement2 | Measurements]) ->
  Gradient = (Measurement2#measurement.value - Measurement1#measurement.value) /
    (Measurement2#measurement.datetime - Measurement1#measurement.datetime),
  MaxGradient = get_gradient([Measurement2 | Measurements]),
  case Gradient > MaxGradient of
    true -> Gradient;
    false -> MaxGradient
  end;
get_gradient(_) -> 0.
