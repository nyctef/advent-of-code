from collections import defaultdict, namedtuple
from itertools import chain, combinations
from pprint import pprint
from pathlib import Path
import re
from typing import Dict, List, NamedTuple, Tuple


class ParsedCave(NamedTuple):
    name: str
    rate: str
    tunnels: List[str]


class TargetLocation(NamedTuple):
    # human-readable (ish) name of this location
    name: str
    # index of this location in World.locations
    index: int
    rate: int
    # = 1<<index
    flag: int


class World(NamedTuple):
    # the time limit for this particular world
    last_minute: int
    # only AA and working valves are stored here
    locations: List[TargetLocation]
    # distances from location X to location Y indexed by TargetLocation.index
    distances: List[List[int]]


class WorldState(NamedTuple):
    # these positions index into World.locations
    my_position: int
    ele_position: int
    # valve state is a bitfield of which World.locations valves are opened
    valve_state: int


class ScorableValveState(NamedTuple):
    # indexes into World.locations
    valve_index: int
    valve_rate: int
    # which minute the valve was turned on
    # the valve scores `valve_rate` points every minute after that one
    on_at_min: int


class ScorableWorldState(NamedTuple):
    valve_states: List[ScorableValveState]
