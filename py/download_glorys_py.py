def sort_dimension(dataset, dim_name):
    """
    Get the values for the specified dimension and verify if they are unsorted. If so, the function sorts them.

    Source: https://help.marine.copernicus.eu/en/articles/7970637-how-to-download-data-for-multiple-points-from-a-csv
    """
    # Get the coordinate values for the specified dimension.
    coordinates = dataset[dim_name].values

    # Check if the coordinates are unsorted.
    if (coordinates[0] >= coordinates[:-1]).all():
        dataset = dataset.sortby(dim_name, ascending=True)
        
    return dataset

def download_glorys_py(dataset, target_data, sel_date):
    """
    Download data from GLORYS product.

    Args:
        dataset: a dataset opened through the Copernicus Marine API (copernicusmarine).
        target_data: the target dataset containing columns `decimalLongitude`, `decimalLatitude` 
        and a column with the site depth (that can be 0, to get surface) named `depth_surface`
        sel_date: selected date

    Returns:
        data frame: The data frame with the requested data.

    Depends:
        xarray, pandas
    """
    
    import xarray as xr
    import pandas as pd

    dataset = sort_dimension(dataset, 'latitude')
    dataset = sort_dimension(dataset, 'longitude')

    lons = xr.DataArray(target_data['decimalLongitude'], dims="z")
    lats = xr.DataArray(target_data['decimalLatitude'], dims="z")

    target_date = pd.to_datetime(sel_date)

    depth_columns = ['depth_surface']
    results = []

    for depth_col in depth_columns:

        depth_coord = xr.DataArray(target_data[depth_col], dims="z")

        selected_data = dataset['thetao'].sel( 
                longitude = lons,
                latitude = lats,
                depth = depth_coord, 
                time = target_date,
                method = 'nearest'
            )
        
        df = selected_data.to_dataframe().rename(columns={'thetao': 'value'}).reset_index()
        df['depth_type'] = depth_col
        df['temp_ID'] = target_data['temp_ID']

        results.append(df)

    # Extract bottom temperature
    selected_data_bottom = dataset['bottomT'].sel( 
            longitude = lons,
            latitude = lats,
            time = target_date,
            method='nearest'
        )
    
    df = selected_data_bottom.to_dataframe().rename(columns={'bottomT': 'value'}).reset_index()
    df['depth_type'] = 'depth_bottom'
    df['temp_ID'] = target_data['temp_ID']

    results.append(df)

    pd.concat(results, axis=1).reset_index()

    result_df = pd.concat(results, ignore_index=True)

    return result_df
