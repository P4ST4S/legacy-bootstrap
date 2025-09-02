"""
Factory Implementation for Dependency Injection

This module implements the factory pattern to manage dependency injection
and system configuration, providing loose coupling and testability.
"""

from typing import Optional, Dict, Any
from interfaces import (
    DataManagerInterface, 
    AccountOperationsInterface, 
    AccountManagementInterface,
    SystemFactoryInterface
)
from data_manager import DataManager
from operations import AccountOperations
from main import AccountManagementSystem


class SystemFactory(SystemFactoryInterface):
    """
    Concrete factory implementation for creating system components.
    
    This factory manages dependency injection and enables different
    configurations for production, testing, and development environments.
    """
    
    def create_data_manager(self, config: Optional[Dict[str, Any]] = None) -> DataManagerInterface:
        """
        Create a data manager instance.
        
        Args:
            config (dict, optional): Configuration parameters
                - data_file (str): Path to data file
                - default_balance (str): Default balance amount
                
        Returns:
            DataManagerInterface: Data manager instance
        """
        if config is None:
            config = {}
        
        data_file = config.get('data_file', 'account_data.json')
        return DataManager(data_file)
    
    def create_operations(self, data_manager: DataManagerInterface) -> AccountOperationsInterface:
        """
        Create an account operations instance.
        
        Args:
            data_manager (DataManagerInterface): Data manager dependency
            
        Returns:
            AccountOperationsInterface: Operations instance
        """
        return AccountOperations(data_manager)
    
    def create_system(self, operations: AccountOperationsInterface) -> AccountManagementInterface:
        """
        Create the main system instance.
        
        Args:
            operations (AccountOperationsInterface): Operations dependency
            
        Returns:
            AccountManagementInterface: System instance
        """
        return AccountManagementSystem(operations)
    
    def create_complete_system(self, config: Optional[Dict[str, Any]] = None) -> AccountManagementInterface:
        """
        Create a complete system with all dependencies wired.
        
        Args:
            config (dict, optional): System configuration
                - data_file (str): Path to data file
                - default_balance (str): Default balance amount
                
        Returns:
            AccountManagementInterface: Complete system instance
        """
        # Create dependencies in order
        data_manager = self.create_data_manager(config)
        operations = self.create_operations(data_manager)
        system = self.create_system(operations)
        
        return system


class TestSystemFactory(SystemFactoryInterface):
    """
    Factory for creating test system configurations.
    
    This factory creates instances optimized for testing,
    with temporary data storage and controlled dependencies.
    """
    
    def __init__(self, test_data_file: str = "test_account_data.json"):
        """
        Initialize test factory.
        
        Args:
            test_data_file (str): Test data file path
        """
        self.test_data_file = test_data_file
    
    def create_data_manager(self, config: Optional[Dict[str, Any]] = None) -> DataManagerInterface:
        """Create a test data manager with temporary storage."""
        return DataManager(self.test_data_file)
    
    def create_operations(self, data_manager: DataManagerInterface) -> AccountOperationsInterface:
        """Create test operations instance."""
        return AccountOperations(data_manager)
    
    def create_system(self, operations: AccountOperationsInterface) -> AccountManagementInterface:
        """Create test system instance."""
        return AccountManagementSystem(operations)
    
    def create_complete_system(self, config: Optional[Dict[str, Any]] = None) -> AccountManagementInterface:
        """Create complete test system."""
        data_manager = self.create_data_manager(config)
        operations = self.create_operations(data_manager)
        system = self.create_system(operations)
        return system


class MockSystemFactory(SystemFactoryInterface):
    """
    Factory for creating mock system configurations.
    
    This factory allows injection of mock dependencies for unit testing.
    """
    
    def __init__(self, 
                 mock_data_manager: Optional[DataManagerInterface] = None,
                 mock_operations: Optional[AccountOperationsInterface] = None):
        """
        Initialize mock factory.
        
        Args:
            mock_data_manager (DataManagerInterface, optional): Mock data manager
            mock_operations (AccountOperationsInterface, optional): Mock operations
        """
        self.mock_data_manager = mock_data_manager
        self.mock_operations = mock_operations
    
    def create_data_manager(self, config: Optional[Dict[str, Any]] = None) -> DataManagerInterface:
        """Return mock data manager if provided, otherwise create real one."""
        if self.mock_data_manager:
            return self.mock_data_manager
        return SystemFactory().create_data_manager(config)
    
    def create_operations(self, data_manager: DataManagerInterface) -> AccountOperationsInterface:
        """Return mock operations if provided, otherwise create real one."""
        if self.mock_operations:
            return self.mock_operations
        return SystemFactory().create_operations(data_manager)
    
    def create_system(self, operations: AccountOperationsInterface) -> AccountManagementInterface:
        """Create system with provided dependencies."""
        return AccountManagementSystem(operations)
    
    def create_complete_system(self, config: Optional[Dict[str, Any]] = None) -> AccountManagementInterface:
        """Create complete system with mocked dependencies."""
        data_manager = self.create_data_manager(config)
        operations = self.create_operations(data_manager)
        system = self.create_system(operations)
        return system


# Convenience function for creating production systems
def create_production_system(data_file: str = "account_data.json") -> AccountManagementInterface:
    """
    Convenience function to create a production system.
    
    Args:
        data_file (str): Path to data file
        
    Returns:
        AccountManagementInterface: Complete production system
    """
    factory = SystemFactory()
    config = {'data_file': data_file}
    return factory.create_complete_system(config)


# Convenience function for creating test systems
def create_test_system(test_data_file: str = "test_account_data.json") -> AccountManagementInterface:
    """
    Convenience function to create a test system.
    
    Args:
        test_data_file (str): Path to test data file
        
    Returns:
        AccountManagementInterface: Complete test system
    """
    factory = TestSystemFactory(test_data_file)
    return factory.create_complete_system()
