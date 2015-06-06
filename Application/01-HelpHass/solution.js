var start = 'H',
    end = 'L';

function flatten_path(path) {
    var flatten = [];
    return flatten.concat.apply(flatten, path).reduce(function(path, station){
        if (path.length === 0 || path[path.length-1] !== station) {
            path.push(station);
        }
        return path;
    }, []);
}

function get_last_station_in_path(path) {
    var flatten = flatten_path(path);
    return flatten.length > 0 ? flatten[flatten.length - 1] : null;
}

function find_shortest_path(connections) {
    var paths = [];
    while (connections.length > 0) {
        var connection = connections.shift(),
            connection_used = false;
        if (connection[0] === start) {
            paths.push([connection]);
            connection_used = true;
        } else {
            paths.forEach(function(path, index) {
                var last_station_in_path = get_last_station_in_path(path);
                if (last_station_in_path === connection[0]) {
                    paths[index].push(connection);
                    connection_used = true;
                }
            });
        }

        if (!connection_used) {
            connections.push(connection); // add to end of queue
        }

    }

    return paths.map(function(path){
        return flatten_path(path);
    }).reduce(function(shortest_path, current_path){
        if (!shortest_path) return current_path;
        return shortest_path.length > current_path.length ? current_path : shortest_path;
    });
}

var trains = [
    ['H', 'F'],
    ['F', 'L'],
    ['H', 'L']
];
console.log('Trains:', trains);
var path = find_shortest_path(trains);
console.log('Shortest path:', path);

trains = [
    ['H', 'A'],
    ['K', 'L'],
    ['S', 'K'],
    ['A', 'S']
];
console.log('Trains:', trains);
path = find_shortest_path(trains);
console.log('Shortest path:', path);