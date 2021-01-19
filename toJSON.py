import json

with open('boxer_date.json') as file:
    lines = []
    for line in file:
        new_line = line.strip()[16:]
        if new_line[-1] == ',':
            new_line = new_line[:-1]
        new_line_json = json.loads(f'{new_line}')
        lines.append(new_line_json)


with open('boxer_date_data.json', 'w') as file:
    json.dump(lines, file, indent=4)