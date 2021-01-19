import json 

with open('boxer_date_data.json') as file:
    data = json.load(file)

with open('boxer_date_data.csv', 'w') as file:
    file.write('Name,Height,Weight,BirthDate\n')
    for person in data:
        file.write(f"{person['title']},{person['ontology/height']},{person['ontology/weight']},{person['ontology/birthDate']}\n")