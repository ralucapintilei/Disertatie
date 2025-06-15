import pandas as pd
import re

# Încarcă fișierul Excel
file_path = 'C:\\Users\\pinti\\OneDrive - FEAA\\Desktop\\disertatie\\Queries.xlsx'
df = pd.read_excel(file_path, sheet_name='Sheet1')

# Definește funcția care analizează fiecare script MongoDB
def count_occurrences(script):
    subqueries = len(re.findall(r'\$lookup.*?pipeline', script, re.DOTALL))
    groupings = script.count('$group')
    filters = script.count('$match')
    joins = script.count('$lookup')
    where_filters = len(re.findall(r'\$match.*?\}', script, re.DOTALL))
    subconsultari = script.count('.aggregate')

    return {
        'Subconsultari ($lookup pipeline)': subqueries,
        'Grupari ($group)': groupings,
        'Filtre ($match)': filters,
        'Nr. Join-uri ($lookup)': joins,
        'Filtre in clauza WHERE': where_filters,
        'Nr. Subconsultari (.aggregate)': subconsultari
    }

# Aplică funcția pe coloana MongoDB Query
analysis_results = df['MongoDB Query'].apply(count_occurrences).apply(pd.Series)

# Adaugă rezultatele în DataFrame-ul original
df_final = pd.concat([df, analysis_results], axis=1)

# Calculează totaluri generale
totals = analysis_results.sum().rename('Total General')

# Adaugă totalul general în DataFrame-ul final
df_final = df_final._append(totals)

# Salvează rezultatele într-un nou Excel
output_path = 'Analiza_totala_queries.xlsx'
df_final.to_excel(output_path, index=False)

print(f"Analiza a fost finalizată și salvată în fișierul: {output_path}")
