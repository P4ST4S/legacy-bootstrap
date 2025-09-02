"""
Interface Abstractions for Account Management System

This module defines the abstract interfaces that decouple the concrete implementations
and enable better testability, flexibility, and maintainability through dependency injection.
"""

from abc import ABC, abstractmethod
from decimal import Decimal
from typing import Optional


class DataManagerInterface(ABC):
    """
    Abstract interface for data management operations.
    
    This interface defines the contract for data persistence operations,
    allowing different implementations (JSON, database, memory, etc.)
    without affecting the business logic.
    """
    
    @abstractmethod
    def read_balance(self) -> Decimal:
        """
        Read the current account balance from storage.
        
        Returns:
            Decimal: Current account balance
            
        Raises:
            Exception: If data cannot be read
        """
        pass
    
    @abstractmethod
    def write_balance(self, balance: Decimal) -> None:
        """
        Write the account balance to storage.
        
        Args:
            balance (Decimal): New balance to store
            
        Raises:
            Exception: If data cannot be written
        """
        pass
    
    @abstractmethod
    def reset_balance(self) -> None:
        """
        Reset balance to default value.
        
        Raises:
            Exception: If reset operation fails
        """
        pass
    
    @abstractmethod
    def get_data_file_path(self) -> str:
        """
        Get the current data storage identifier/path.
        
        Returns:
            str: Storage identifier (file path, database connection, etc.)
        """
        pass


class AccountOperationsInterface(ABC):
    """
    Abstract interface for account operations.
    
    This interface defines the contract for business operations,
    enabling different implementations and better testing through mocking.
    """
    
    @abstractmethod
    def view_balance(self) -> Decimal:
        """
        Display and return the current account balance.
        
        Returns:
            Decimal: Current account balance
        """
        pass
    
    @abstractmethod
    def credit_account(self, amount: Optional[Decimal] = None) -> bool:
        """
        Credit the account with a specified amount.
        
        Args:
            amount (Decimal, optional): Amount to credit. If None, prompts user.
            
        Returns:
            bool: True if operation successful, False otherwise
        """
        pass
    
    @abstractmethod
    def debit_account(self, amount: Optional[Decimal] = None) -> bool:
        """
        Debit the account with a specified amount.
        
        Args:
            amount (Decimal, optional): Amount to debit. If None, prompts user.
            
        Returns:
            bool: True if operation successful, False otherwise
        """
        pass
    
    @abstractmethod
    def get_current_balance(self) -> Decimal:
        """
        Get current balance without displaying it.
        
        Returns:
            Decimal: Current account balance
        """
        pass
    
    @abstractmethod
    def reset_account(self) -> None:
        """
        Reset account to default balance.
        """
        pass


class AccountManagementInterface(ABC):
    """
    Abstract interface for the main application management.
    
    This interface defines the contract for the main application,
    enabling different UI implementations (console, web, GUI, etc.).
    """
    
    @abstractmethod
    def display_menu(self) -> None:
        """Display the main menu options."""
        pass
    
    @abstractmethod
    def get_user_choice(self) -> str:
        """
        Get user menu choice with input validation.
        
        Returns:
            str: User's menu choice
        """
        pass
    
    @abstractmethod
    def execute_choice(self, choice: str) -> None:
        """
        Execute the selected menu option.
        
        Args:
            choice (str): User's menu choice
        """
        pass
    
    @abstractmethod
    def run(self) -> None:
        """Main application loop."""
        pass


class SystemFactoryInterface(ABC):
    """
    Abstract factory interface for creating system components.
    
    This interface enables different system configurations and
    dependency injection strategies.
    """
    
    @abstractmethod
    def create_data_manager(self, config: Optional[dict] = None) -> DataManagerInterface:
        """
        Create a data manager instance.
        
        Args:
            config (dict, optional): Configuration parameters
            
        Returns:
            DataManagerInterface: Data manager instance
        """
        pass
    
    @abstractmethod
    def create_operations(self, data_manager: DataManagerInterface) -> AccountOperationsInterface:
        """
        Create an account operations instance.
        
        Args:
            data_manager (DataManagerInterface): Data manager dependency
            
        Returns:
            AccountOperationsInterface: Operations instance
        """
        pass
    
    @abstractmethod
    def create_system(self, operations: AccountOperationsInterface) -> AccountManagementInterface:
        """
        Create the main system instance.
        
        Args:
            operations (AccountOperationsInterface): Operations dependency
            
        Returns:
            AccountManagementInterface: System instance
        """
        pass
    
    @abstractmethod
    def create_complete_system(self, config: Optional[dict] = None) -> AccountManagementInterface:
        """
        Create a complete system with all dependencies wired.
        
        Args:
            config (dict, optional): System configuration
            
        Returns:
            AccountManagementInterface: Complete system instance
        """
        pass
