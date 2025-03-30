{{ config(
    materialized='table',
    partition_by={
      "field": "game_date",
      "data_type": "date",
      "granularity": "day"
    },
    cluster_by = "player_id"
)}}
WITH player_game_data AS (
    select 
    -- Primary keys
        athlete_id AS player_id,
        game_id,
    -- Dates
        CAST(game_date AS DATE) AS game_date,
    -- Player info
        athlete_display_name AS player_name,
        athlete_short_name AS player_short_name,
        athlete_position_name AS player_position,
        athlete_position_abbreviation AS player_position_abbr,
        athlete_headshot_href AS player_image,
    -- Team info
        team_id,
        team_name,
    -- Game context
        season,
        season_type,
    -- Game stats
        minutes,
        points,
        field_goals_made,
        field_goals_attempted,
        
        three_point_field_goals_made,
        three_point_field_goals_attempted,
        
        free_throws_made,
        free_throws_attempted,

        rebounds,
        assists,
        steals,
        blocks,
        fouls,
        -- Status flags
        did_not_play,

        ROW_NUMBER() OVER (
        PARTITION BY athlete_id, game_id 
        ORDER BY game_date DESC  -- Assuming you want the most recent record in case of duplicates
        ) AS row_num

    from {{ source('player_db', 'player_table') }}
)

SELECT 
    * EXCEPT(row_num) 
FROM 
    player_game_data 
WHERE 
    row_num = 1