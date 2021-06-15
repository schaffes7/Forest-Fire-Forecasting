import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


# ==================================================================
# IMPORT FILE
# ==================================================================
fpath = "amazon.csv"
df = pd.read_csv(fpath, encoding = 'ISO-8859-1')


# ==================================================================
# FEATURE CREATION
# ==================================================================
months = {'Janeiro':'January',
          'Fevereiro':'February',
          'Março':'March',
          'Abril':'April',
          'Maio':'May',
          'Junho':'June',
          'Julho':'July',
          'Agosto':'August',
          'Setembro':'September',
          'Outubro':'October',
          'Novembro':'November',
          'Dezembro':'December'}

states = {'Acre':'',
 'Alagoas':'Alagoas',
 'Amapa':'Amapá',
 'Amazonas':'Amazonas',
 'Bahia':'Bahia',
 'Ceara':'Ceará',
 'Distrito Federal':'Distrito Federal',
 'Espirito Santo':'Espírito Santo',
 'Goias':'Goiás',
 'Maranhao':'Maranhão',
 'Mato Grosso':'Mato Grosso',
 'Minas Gerais':'Minas Gerais',
 'Paraiba':'Paraíba',
 'Pará':'Pará',
 'Pernambuco':'Pernambuco',
 'Piau':'Piauí',
 'Rio':'Rio',
 'Rondonia':'Rondônia',
 'Roraima':'Roraima',
 'Santa Catarina':'Santa Catarina',
 'Sao Paulo':'São Paulo',
 'Sergipe':'Sergipe',
 'Tocantins':'Tocantins'}

for val in months.keys():
    df['month'][df['month'] == val] = months[val]

df['country'] = 'Brazil'
df['year'] = df['year'].astype(str)
df['date'] = df[['month','year']].apply(lambda x: ' '.join(x), axis=1)

df = df.drop_duplicates()

print('Correcting Decimals')
for i in range(len(df)):
    val = df['number'].iloc[i]
    if round(val) != val:
        df['number'].iloc[i] = round(val*1000)
df['number'] = df['number'].astype(int)


# ==================================================================
# SLICE & SAVE DATASETS
# ==================================================================
df.to_csv("amazon_clean.csv", index = False, encoding = 'iso-8859-1')

all_fires = []
for u in list(np.unique(df['date'])):
    row = [u, np.sum(df['number'][df['date'] == u])]
    all_fires.append(row)

all_fires = pd.DataFrame(all_fires, columns = ['date','totals'])
all_fires['date'] = pd.to_datetime(all_fires['date'],format = '%B %Y')
all_fires = all_fires.sort_values(by = 'date')
all_fires.to_csv("amazon_allfires.csv", index = False)


# ==================================================================
# PLOT DATA
# ==================================================================
plt.figure(figsize = (12,8))
plt.plot(all_fires.totals, color = 'red')
plt.title('Forest Fire Time Series', fontsize = 22)
plt.xlabel('Month', fontsize = 18)
plt.ylabel('Monthly Forest Fire Count', fontsize = 18)
plt.show()
