# Advanced HMG App README

## Overview
This is a desktop application built with Harbour MiniGUI (HMG) and SQLite, providing a user interface to manage and export data from a SQLite database. It features a login system, data grid display, and export functionality to CSV and XML formats.

## Prerequisites
- **Harbour Compiler**: Ensure Harbour is installed.
- **MiniGUI Library**: Install the HMG library for GUI components.
- **SQLite3**: Ensure the `hbsqlit3` library is available for database operations.

## Installation
1. Clone or download the project files.
2. Ensure all `.prg` files (`main.prg`, `ui.prg`, `login.prg`, `data.prg`, `events.prg`) are in the same directory.
3. Compile the application using the Harbour compiler.
4. Ensure `app.ini` and `people.db` are writable in the application directory.

## Usage
1. **Run the Application**:
   - Execute the compiled `.exe` file.
   - The login window appears.

2. **Login**:
   - Enter a username (default: "Guest" from `app.ini`).
   - Click "Login" to proceed. If the username is empty, an error message will appear.

3. **Main Interface**:
   - **Dashboard Tab**:
     - Displays a grid with data (ID, Name, Age) from `people.db`.
     - Auto-refreshes every 5 seconds.
     - Buttons to export data to `people.csv` or `people.xml`.
   - **Settings Tab**:
     - Modify the username.
     - Click "Save Settings" to update `app.ini`.

4. **Export Data**:
   - **CSV Export**: Click "Export CSV" to save data to `people.csv`.
   - **XML Export**: Click "Export XML" to save data to `people.xml`.

5. **Database**:
   - The app creates `people.db` if it doesn't exist, with a sample table (`people`) containing three records.
   - Data is displayed in the grid and can be exported.

## Files
- `main.prg`: Entry point, launches the login window.
- `ui.prg`: Defines the main GUI (tabs, grid, buttons).
- `login.prg`: Handles the login window and user input.
- `data.prg`: Manages app settings (`app.ini`) and database initialization/export functions.
- `events.prg`: Handles grid population and auto-refresh.

## Notes
- The app uses SQLite for data storage and MiniGUI for the interface.
- Ensure write permissions for `app.ini`, `people.db`, `people.csv`, and `people.xml`.
- The grid alternates row colors for readability.
- No external image files are used; `appicon` is assumed to be available.