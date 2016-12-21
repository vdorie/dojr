"""One of script for munging the DOJ crimes and clearances dataset."""
import pandas as pd


if __name__ == '__main__':
    infile = '~/12_13_2015/crimes_clearances_1982-2014.csv'
    df = pd.read_csv(infile)
    # O joy (le vocative declension)
    remove_quote = lambda x: x.split("'")[1]
    df['ncic_code'] = df['ncic_code'].apply(remove_quote)
    df['bcs_code'] = df['bcs_code'].apply(remove_quote)
    df['year'] = df['year'].astype(int)
    df['month'] = df['month'].astype(int)
    errant_col = 'Unnamed: 0'
    if errant_col in df.columns:
        del df[errant_col]
    common_cols = 4
    reshape_cols = df.iloc[:, common_cols + 1:]
    result = []
    for col in reshape_cols.columns:
        print(col)
        tmp_result = df.iloc[:, :common_cols]
        tmp_result['crime_type'] = col
        tmp_result['amount'] = reshape_cols[col]
        result.append(tmp_result)
    result = pd.concat(result)
    del df
    null = 'NULL'
    result = result.fillna(null)
    result= result.sort_values(by=['year', 'month', 'ncic_code', 'bcs_code'])
