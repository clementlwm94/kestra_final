{{ config(materialized='table') }}

WITH team_data AS (
    SELECT * FROM {{ ref('stg_team') }}
),

latest_team_info AS (
    SELECT
        team_id,
        team_name,
        team_location,
        team_abbreviation,
        team_full_name,
        team_short_display_name,
        team_slug,
        team_color,
        team_alternate_color,
        team_logo,
        ROW_NUMBER() OVER (
            PARTITION BY team_id 
            ORDER BY game_date DESC
        ) AS row_num
    FROM team_data
),

-- Select only the most recent record for each team
unique_teams AS (
    SELECT
        team_id,
        team_name,
        team_location,
        team_abbreviation,
        team_full_name,
        team_short_display_name,
        team_slug,
        team_color,
        team_alternate_color,
        team_logo
    FROM latest_team_info
    WHERE row_num = 1
),

-- Get team stats
team_stats AS (
    SELECT
        team_id,
        
        -- Game counts
        COUNT(DISTINCT game_id) AS games_played,
        SUM(CASE WHEN is_winner = TRUE THEN 1 ELSE 0 END) AS wins,
        SUM(CASE WHEN is_winner = FALSE THEN 1 ELSE 0 END) AS losses,
        SUM(CASE WHEN home_away = 'home' THEN 1 ELSE 0 END) AS home_games,
        SUM(CASE WHEN home_away = 'away' THEN 1 ELSE 0 END) AS away_games,
        SUM(CASE WHEN home_away = 'home' AND is_winner = TRUE THEN 1 ELSE 0 END) AS home_wins,
        SUM(CASE WHEN home_away = 'away' AND is_winner = TRUE THEN 1 ELSE 0 END) AS away_wins,
        
        -- Team averages
        ROUND(AVG(team_score), 1) AS avg_points_scored,
        ROUND(AVG(field_goal_pct), 1) AS avg_fg_pct,
        ROUND(AVG(three_point_field_goal_pct), 1) AS avg_3pt_pct,
        ROUND(AVG(free_throw_pct), 1) AS avg_ft_pct,
        ROUND(AVG(assists), 1) AS avg_assists,
        ROUND(AVG(total_rebounds), 1) AS avg_rebounds,
        ROUND(AVG(steals), 1) AS avg_steals,
        ROUND(AVG(blocks), 1) AS avg_blocks,
        ROUND(AVG(turnovers), 1) AS avg_turnovers,
        ROUND(AVG(fouls), 1) AS avg_fouls,
        
        -- Season extremes
        MAX(team_score) AS highest_points_scored,
        MIN(team_score) AS lowest_points_scored,
        MAX(largest_lead) AS largest_lead,
        
    FROM team_data
    GROUP BY team_id
),

-- Calculate win percentage
team_records AS (
    SELECT
        team_id,
        games_played,
        wins,
        losses,
        home_games,
        away_games,
        home_wins,
        away_wins,
        CASE
            WHEN games_played > 0 THEN ROUND(wins / games_played * 100, 1)
            ELSE 0
        END AS win_percentage,
        CASE
            WHEN home_games > 0 THEN ROUND(home_wins / home_games * 100, 1)
            ELSE 0
        END AS home_win_percentage,
        CASE
            WHEN away_games > 0 THEN ROUND(away_wins / away_games * 100, 1)
            ELSE 0
        END AS away_win_percentage,
        avg_points_scored,
        avg_fg_pct,
        avg_3pt_pct,
        avg_ft_pct,
        avg_assists,
        avg_rebounds,
        avg_steals,
        avg_blocks,
        avg_turnovers,
        avg_fouls,
        highest_points_scored,
        lowest_points_scored,
        largest_lead,
    FROM team_stats
)

SELECT
    t.team_name,
    t.team_location,
    t.team_abbreviation,
    t.team_full_name,
    t.team_short_display_name,
    t.team_slug,
    t.team_logo,
    r.games_played,
    r.wins,
    r.losses,
    r.home_games,
    r.away_games,
    r.win_percentage,
    r.home_win_percentage,
    r.away_win_percentage,
    r.avg_points_scored,
    r.avg_fg_pct,
    r.avg_3pt_pct,
    r.avg_ft_pct,
    r.avg_assists,
    r.avg_rebounds,
    r.avg_steals,
    r.avg_blocks,
    r.avg_turnovers,
    r.avg_fouls,
    r.highest_points_scored,
    r.lowest_points_scored,
    r.largest_lead,
    CURRENT_TIMESTAMP() AS updated_at
FROM unique_teams t
JOIN team_records r ON t.team_id = r.team_id