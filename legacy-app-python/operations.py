"""
Operations Module - Modernized version of operations.cob

This module handles all account operations including credit, debit, and balance inquiries.
Replaces the COBOL operations logic with modern Python implementations.
"""

from decimal import Decimal, InvalidOperation
from typing import Optional
from interfaces import DataManagerInterface, AccountOperationsInterface


class AccountOperations(AccountOperationsInterface):
    """
    Handles account operations such as credit, debit, and balance viewing.
    
    This class replaces the COBOL Operations program with modern Python
    methods while maintaining the same business logic.
    """
    
    def __init__(self, data_manager: DataManagerInterface):
        """
        Initialize AccountOperations with a DataManager instance.
        
        Args:
            data_manager (DataManagerInterface): Data manager instance (required for DI).
        """
        if data_manager is None:
            raise ValueError("DataManager dependency is required")
        self.data_manager = data_manager
    
    def view_balance(self) -> Decimal:
        """
        Display and return the current account balance.
        
        Equivalent to COBOL operation when OPERATION-TYPE = 'TOTAL '
        
        Returns:
            Decimal: Current account balance
        """
        try:
            current_balance = self.data_manager.read_balance()
            print(f"Current balance: {current_balance:.2f}")
            return current_balance
        except Exception as e:
            print(f"Error retrieving balance: {e}")
            return Decimal('0.00')
    
    def credit_account(self, amount: Optional[Decimal] = None) -> bool:
        """
        Credit the account with a specified amount.
        
        Equivalent to COBOL operation when OPERATION-TYPE = 'CREDIT'
        
        Args:
            amount (Decimal, optional): Amount to credit. If None, prompts user.
            
        Returns:
            bool: True if operation successful, False otherwise
        """
        try:
            if amount is None:
                amount = self._get_amount_from_user("Enter credit amount: ")
                if amount is None:
                    return False
            
            if amount < 0:
                print("Error: Credit amount cannot be negative.")
                return False
            
            current_balance = self.data_manager.read_balance()
            new_balance = current_balance + amount
            
            self.data_manager.write_balance(new_balance)
            print(f"Amount credited. New balance: {new_balance:.2f}")
            return True
            
        except Exception as e:
            print(f"Error processing credit: {e}")
            return False
    
    def debit_account(self, amount: Optional[Decimal] = None) -> bool:
        """
        Debit the account with a specified amount.
        
        Equivalent to COBOL operation when OPERATION-TYPE = 'DEBIT '
        Includes insufficient funds checking like the original COBOL.
        
        Args:
            amount (Decimal, optional): Amount to debit. If None, prompts user.
            
        Returns:
            bool: True if operation successful, False otherwise
        """
        try:
            if amount is None:
                amount = self._get_amount_from_user("Enter debit amount: ")
                if amount is None:
                    return False
            
            if amount < 0:
                print("Error: Debit amount cannot be negative.")
                return False
            
            current_balance = self.data_manager.read_balance()
            
            # Check for sufficient funds (same logic as COBOL)
            if current_balance >= amount:
                new_balance = current_balance - amount
                self.data_manager.write_balance(new_balance)
                print(f"Amount debited. New balance: {new_balance:.2f}")
                return True
            else:
                print("Insufficient funds for this debit.")
                return False
                
        except Exception as e:
            print(f"Error processing debit: {e}")
            return False
    
    def _get_amount_from_user(self, prompt: str) -> Optional[Decimal]:
        """
        Get amount input from user with validation.
        
        Replaces COBOL ACCEPT AMOUNT with proper input validation.
        
        Args:
            prompt (str): Prompt message for user input
            
        Returns:
            Decimal: Valid amount entered by user, or None if invalid
        """
        try:
            user_input = input(prompt).strip()
            if not user_input:
                print("Error: No amount entered.")
                return None
                
            amount = Decimal(user_input)
            
            if amount < 0:
                print("Error: Amount cannot be negative.")
                return None
                
            return amount
            
        except InvalidOperation:
            print("Error: Invalid amount format. Please enter a valid number.")
            return None
        except KeyboardInterrupt:
            print("\nOperation cancelled by user.")
            return None
    
    def get_current_balance(self) -> Decimal:
        """
        Get current balance without displaying it.
        
        Utility method for programmatic access to balance.
        
        Returns:
            Decimal: Current account balance
        """
        return self.data_manager.read_balance()
    
    def reset_account(self) -> None:
        """
        Reset account to default balance.
        
        Utility method for testing and system reset.
        """
        self.data_manager.reset_balance()
        print("Account reset to default balance.")
