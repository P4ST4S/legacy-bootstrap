"""
Data Manager Module - Modernized version of data.cob

This module handles the persistent storage and retrieval of account balance data.
Replaces the COBOL data storage functionality with modern Python approaches.
"""

import json
import os
from decimal import Decimal
from typing import Optional
from interfaces import DataManagerInterface


class DataManager(DataManagerInterface):
    """
    Manages account balance data with persistent storage.
    
    This class replaces the COBOL DATA DIVISION functionality with modern
    file-based persistence using JSON format for better data integrity.
    """
    
    def __init__(self, data_file: str = "account_data.json"):
        """
        Initialize the DataManager with a specified data file.
        
        Args:
            data_file (str): Path to the data storage file
        """
        self.data_file = data_file
        self.default_balance = Decimal('1000.00')
        self._ensure_data_file_exists()
    
    def _ensure_data_file_exists(self) -> None:
        """
        Ensure the data file exists with default values if it doesn't.
        
        This replaces the COBOL WORKING-STORAGE SECTION initialization.
        """
        if not os.path.exists(self.data_file):
            self._save_balance(self.default_balance)
    
    def read_balance(self) -> Decimal:
        """
        Read the current account balance from storage.
        
        Equivalent to COBOL: CALL 'DataProgram' USING 'READ', BALANCE
        
        Returns:
            Decimal: Current account balance
            
        Raises:
            FileNotFoundError: If data file cannot be found
            ValueError: If data file contains invalid data
        """
        try:
            with open(self.data_file, 'r', encoding='utf-8') as file:
                data = json.load(file)
                balance_str = data.get('balance', str(self.default_balance))
                return Decimal(balance_str)
        except (FileNotFoundError, json.JSONDecodeError, ValueError) as e:
            print(f"Warning: Error reading balance data: {e}")
            print("Resetting to default balance.")
            self._save_balance(self.default_balance)
            return self.default_balance
    
    def write_balance(self, balance: Decimal) -> None:
        """
        Write the account balance to storage.
        
        Equivalent to COBOL: CALL 'DataProgram' USING 'WRITE', BALANCE
        
        Args:
            balance (Decimal): New balance to store
            
        Raises:
            IOError: If unable to write to data file
        """
        self._save_balance(balance)
    
    def _save_balance(self, balance: Decimal) -> None:
        """
        Internal method to save balance data to file.
        
        Args:
            balance (Decimal): Balance to save
        """
        try:
            data = {
                'balance': str(balance),
                'last_updated': str(Decimal('0'))  # Could be enhanced with timestamp
            }
            with open(self.data_file, 'w', encoding='utf-8') as file:
                json.dump(data, file, indent=2)
        except IOError as e:
            raise IOError(f"Unable to save balance data: {e}") from e
    
    def reset_balance(self) -> None:
        """
        Reset balance to default value.
        
        Useful for testing and system resets.
        """
        self._save_balance(self.default_balance)
    
    def get_data_file_path(self) -> str:
        """
        Get the current data file path.
        
        Returns:
            str: Path to the data file
        """
        return self.data_file
