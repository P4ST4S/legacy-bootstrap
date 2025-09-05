"""
Unit Tests for the Account Management System

This module contains comprehensive unit tests for all components
of the modernized Python application, following the test plan
from the original COBOL project.
"""

import unittest
import os
import tempfile
from decimal import Decimal
from unittest.mock import patch
from io import StringIO

# Import our modules
from interfaces import DataManagerInterface, AccountOperationsInterface, AccountManagementInterface
from data_manager import DataManager
from operations import AccountOperations
from main import AccountManagementSystem
from factory import SystemFactory, TestSystemFactory as FactoryTestSystemFactory, create_test_system


class TestDataManager(unittest.TestCase):
    """Test cases for DataManager class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.test_file = tempfile.NamedTemporaryFile(delete=False, suffix='.json')
        self.test_file.close()
        self.data_manager = DataManager(self.test_file.name)
    
    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.test_file.name):
            os.unlink(self.test_file.name)
    
    def test_initial_balance(self):
        """Test that initial balance is set correctly."""
        balance = self.data_manager.read_balance()
        self.assertEqual(balance, Decimal('1000.00'))
    
    def test_write_and_read_balance(self):
        """Test writing and reading balance."""
        new_balance = Decimal('1500.75')
        self.data_manager.write_balance(new_balance)
        read_balance = self.data_manager.read_balance()
        self.assertEqual(read_balance, new_balance)
    
    def test_file_not_found_handling(self):
        """Test handling of missing data file."""
        # Remove the file
        os.unlink(self.test_file.name)
        
        # Should create new file with default balance
        balance = self.data_manager.read_balance()
        self.assertEqual(balance, Decimal('1000.00'))
        self.assertTrue(os.path.exists(self.test_file.name))
    
    def test_reset_balance(self):
        """Test balance reset functionality."""
        # Change balance first
        self.data_manager.write_balance(Decimal('500.00'))
        
        # Reset and verify
        self.data_manager.reset_balance()
        balance = self.data_manager.read_balance()
        self.assertEqual(balance, Decimal('1000.00'))
    
    def test_invalid_json_handling(self):
        """Test handling of corrupted data file."""
        # Write invalid JSON
        with open(self.test_file.name, 'w') as f:
            f.write("invalid json content")
        
        # Should reset to default balance
        balance = self.data_manager.read_balance()
        self.assertEqual(balance, Decimal('1000.00'))


class TestAccountOperations(unittest.TestCase):
    """Test cases for AccountOperations class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.test_file = tempfile.NamedTemporaryFile(delete=False, suffix='.json')
        self.test_file.close()
        self.data_manager = DataManager(self.test_file.name)
        self.operations = AccountOperations(self.data_manager)
    
    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.test_file.name):
            os.unlink(self.test_file.name)
    
    @patch('sys.stdout', new_callable=StringIO)
    def test_view_balance(self, mock_stdout):
        """Test TC-1.1: View Current Balance."""
        balance = self.operations.view_balance()
        output = mock_stdout.getvalue()
        
        self.assertEqual(balance, Decimal('1000.00'))
        self.assertIn("Current balance: 1000.00", output)
    
    def test_credit_account_valid_amount(self):
        """Test TC-2.1: Credit Account with Valid Amount."""
        credit_amount = Decimal('100.00')
        result = self.operations.credit_account(credit_amount)
        
        self.assertTrue(result)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, Decimal('1100.00'))
    
    def test_credit_account_zero_amount(self):
        """Test TC-2.2: Credit Account with Zero Amount."""
        credit_amount = Decimal('0.00')
        initial_balance = self.operations.get_current_balance()
        
        result = self.operations.credit_account(credit_amount)
        
        self.assertTrue(result)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, initial_balance)
    
    def test_credit_account_negative_amount(self):
        """Test credit with negative amount (should fail)."""
        credit_amount = Decimal('-50.00')
        initial_balance = self.operations.get_current_balance()
        
        result = self.operations.credit_account(credit_amount)
        
        self.assertFalse(result)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, initial_balance)
    
    def test_debit_account_valid_amount(self):
        """Test TC-3.1: Debit Account with Valid Amount."""
        debit_amount = Decimal('50.00')
        result = self.operations.debit_account(debit_amount)
        
        self.assertTrue(result)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, Decimal('950.00'))
    
    @patch('sys.stdout', new_callable=StringIO)
    def test_debit_account_insufficient_funds(self, mock_stdout):
        """Test TC-3.2: Debit Account with Amount Greater Than Balance."""
        debit_amount = Decimal('2000.00')
        initial_balance = self.operations.get_current_balance()
        
        result = self.operations.debit_account(debit_amount)
        output = mock_stdout.getvalue()
        
        self.assertFalse(result)
        self.assertIn("Insufficient funds", output)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, initial_balance)
    
    def test_debit_account_zero_amount(self):
        """Test TC-3.3: Debit Account with Zero Amount."""
        debit_amount = Decimal('0.00')
        initial_balance = self.operations.get_current_balance()
        
        result = self.operations.debit_account(debit_amount)
        
        self.assertTrue(result)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, initial_balance)
    
    def test_debit_account_negative_amount(self):
        """Test debit with negative amount (should fail)."""
        debit_amount = Decimal('-50.00')
        initial_balance = self.operations.get_current_balance()
        
        result = self.operations.debit_account(debit_amount)
        
        self.assertFalse(result)
        new_balance = self.operations.get_current_balance()
        self.assertEqual(new_balance, initial_balance)
    
    @patch('builtins.input', return_value='150.50')
    def test_get_amount_from_user_valid(self, mock_input):
        """Test user amount input with valid data."""
        amount = self.operations._get_amount_from_user("Enter amount: ")
        self.assertEqual(amount, Decimal('150.50'))
    
    @patch('builtins.input', return_value='invalid')
    def test_get_amount_from_user_invalid(self, mock_input):
        """Test user amount input with invalid data."""
        amount = self.operations._get_amount_from_user("Enter amount: ")
        self.assertIsNone(amount)
    
    @patch('builtins.input', return_value='')
    def test_get_amount_from_user_empty(self, mock_input):
        """Test user amount input with empty data."""
        amount = self.operations._get_amount_from_user("Enter amount: ")
        self.assertIsNone(amount)


class TestAccountManagementSystem(unittest.TestCase):
    """Test cases for AccountManagementSystem class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.test_file = tempfile.NamedTemporaryFile(delete=False, suffix='.json')
        self.test_file.close()
        data_manager = DataManager(self.test_file.name)
        operations = AccountOperations(data_manager)
        self.system = AccountManagementSystem(operations)
    
    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.test_file.name):
            os.unlink(self.test_file.name)
    
    @patch('sys.stdout', new_callable=StringIO)
    def test_display_menu(self, mock_stdout):
        """Test menu display functionality."""
        self.system.display_menu()
        output = mock_stdout.getvalue()
        
        self.assertIn("Account Management System", output)
        self.assertIn("1. View Balance", output)
        self.assertIn("2. Credit Account", output)
        self.assertIn("3. Debit Account", output)
        self.assertIn("4. Exit", output)
    
    @patch('builtins.input', return_value='1')
    def test_get_user_choice_valid(self, mock_input):
        """Test valid user choice input."""
        choice = self.system.get_user_choice()
        self.assertEqual(choice, '1')
    
    @patch('builtins.input', side_effect=['5', '2'])
    @patch('sys.stdout', new_callable=StringIO)
    def test_get_user_choice_invalid_then_valid(self, mock_stdout, mock_input):
        """Test invalid then valid user choice input."""
        choice = self.system.get_user_choice()
        output = mock_stdout.getvalue()
        
        self.assertEqual(choice, '2')
        self.assertIn("Invalid choice", output)
    
    def test_execute_choice_view_balance(self):
        """Test executing view balance choice."""
        self.system.execute_choice('1')
        # Should not raise any exceptions
    
    def test_execute_choice_exit(self):
        """Test TC-4.1: Exit the Application."""
        initial_flag = self.system.continue_flag
        self.assertTrue(initial_flag)
        
        self.system.execute_choice('4')
        
        self.assertFalse(self.system.continue_flag)


class TestIntegration(unittest.TestCase):
    """Integration tests for the complete system."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.test_file = tempfile.NamedTemporaryFile(delete=False, suffix='.json')
        self.test_file.close()
        
    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.test_file.name):
            os.unlink(self.test_file.name)
    
    def test_complete_transaction_flow(self):
        """Test a complete flow of operations."""
        # Initialize system using factory
        factory = FactoryTestSystemFactory(self.test_file.name)
        system = factory.create_complete_system()
        
        # Get operations from the system
        operations = system.operations
        
        # Check initial balance
        balance = operations.view_balance()
        self.assertEqual(balance, Decimal('1000.00'))
        
        # Credit account
        result = operations.credit_account(Decimal('200.00'))
        self.assertTrue(result)
        
        # Check new balance
        balance = operations.get_current_balance()
        self.assertEqual(balance, Decimal('1200.00'))
        
        # Debit account
        result = operations.debit_account(Decimal('150.00'))
        self.assertTrue(result)
        
        # Check final balance
        balance = operations.get_current_balance()
        self.assertEqual(balance, Decimal('1050.00'))
    
    def test_persistence_across_instances(self):
        """Test that data persists across different instances."""
        # Create first instance and modify balance using factory
        factory1 = FactoryTestSystemFactory(self.test_file.name)
        system1 = factory1.create_complete_system()
        operations1 = system1.operations
        operations1.credit_account(Decimal('300.00'))
        
        # Create second instance and check balance using factory
        factory2 = FactoryTestSystemFactory(self.test_file.name)
        system2 = factory2.create_complete_system()
        operations2 = system2.operations
        balance = operations2.get_current_balance()
        
        self.assertEqual(balance, Decimal('1300.00'))

    def test_overdraft_then_valid_debit(self):
        """End-to-end: overdraft should fail, then a valid debit should succeed."""
        factory = FactoryTestSystemFactory(self.test_file.name)
        system = factory.create_complete_system()
        ops = system.operations

        # Starting balance
        start = ops.get_current_balance()

        # Overdraft attempt
        self.assertFalse(ops.debit_account(Decimal('50000.00')))
        self.assertEqual(ops.get_current_balance(), start)

        # Valid debit
        self.assertTrue(ops.debit_account(Decimal('25.00')))
        self.assertEqual(ops.get_current_balance(), start - Decimal('25.00'))

    def test_corrupted_data_resets_and_allows_operations(self):
        """End-to-end: corrupted JSON should reset to default and operations still work."""
        # Corrupt the file before system creation
        with open(self.test_file.name, 'w') as f:
            f.write('{not: valid json')

        factory = FactoryTestSystemFactory(self.test_file.name)
        system = factory.create_complete_system()
        ops = system.operations

        # Should reset to default 1000.00
        self.assertEqual(ops.view_balance(), Decimal('1000.00'))

        # Operations proceed normally
        self.assertTrue(ops.credit_account(Decimal('10.25')))
        self.assertEqual(ops.get_current_balance(), Decimal('1010.25'))

    def test_long_transaction_sequence(self):
        """End-to-end: multiple credits/debits with cents keep precise Decimal math."""
        factory = FactoryTestSystemFactory(self.test_file.name)
        system = factory.create_complete_system()
        ops = system.operations

        # Start from 1000.00
        self.assertEqual(ops.get_current_balance(), Decimal('1000.00'))

        # Sequence
        self.assertTrue(ops.credit_account(Decimal('0.10')))
        self.assertTrue(ops.credit_account(Decimal('0.20')))
        self.assertTrue(ops.debit_account(Decimal('0.05')))
        self.assertTrue(ops.credit_account(Decimal('99.75')))
        self.assertTrue(ops.debit_account(Decimal('50.00')))

        # Final expected balance: 1000.00 + 0.10 + 0.20 - 0.05 + 99.75 - 50.00 = 1049.999999... -> exact Decimal 1049.999999? No, precise adds -> 1049.999999? let's compute
        expected = Decimal('1000.00') + Decimal('0.10') + Decimal('0.20') - Decimal('0.05') + Decimal('99.75') - Decimal('50.00')
        self.assertEqual(ops.get_current_balance(), expected)


class TestFactoryImplementation(unittest.TestCase):
    """Test cases for SystemFactory and dependency injection."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.test_file = tempfile.NamedTemporaryFile(delete=False, suffix='.json')
        self.test_file.close()
        self.factory = SystemFactory()
    
    def tearDown(self):
        """Clean up test fixtures."""
        if os.path.exists(self.test_file.name):
            os.unlink(self.test_file.name)
    
    def test_create_data_manager(self):
        """Test factory creates data manager correctly."""
        config = {'data_file': self.test_file.name}
        data_manager = self.factory.create_data_manager(config)
        
        self.assertIsInstance(data_manager, DataManagerInterface)
        self.assertEqual(data_manager.get_data_file_path(), self.test_file.name)
    
    def test_create_operations(self):
        """Test factory creates operations correctly."""
        data_manager = DataManager(self.test_file.name)
        operations = self.factory.create_operations(data_manager)
        
        self.assertIsInstance(operations, AccountOperationsInterface)
        self.assertEqual(operations.data_manager, data_manager)
    
    def test_create_system(self):
        """Test factory creates system correctly."""
        data_manager = DataManager(self.test_file.name)
        operations = AccountOperations(data_manager)
        system = self.factory.create_system(operations)
        
        self.assertIsInstance(system, AccountManagementInterface)
        self.assertEqual(system.operations, operations)
    
    def test_create_complete_system(self):
        """Test factory creates complete system with all dependencies."""
        config = {'data_file': self.test_file.name}
        system = self.factory.create_complete_system(config)
        
        self.assertIsInstance(system, AccountManagementInterface)
        self.assertIsInstance(system.operations, AccountOperationsInterface)
        self.assertIsInstance(system.operations.data_manager, DataManagerInterface)
    
    def test_test_system_factory(self):
        """Test TestSystemFactory creates test configuration."""
        test_factory = FactoryTestSystemFactory(self.test_file.name)
        system = test_factory.create_complete_system()
        
        self.assertIsInstance(system, AccountManagementInterface)
        # Test that it uses the specified test file
        self.assertEqual(system.operations.data_manager.get_data_file_path(), self.test_file.name)
    
    def test_convenience_functions(self):
        """Test convenience functions work correctly."""
        system = create_test_system(self.test_file.name)
        
        self.assertIsInstance(system, AccountManagementInterface)
        self.assertIsInstance(system.operations, AccountOperationsInterface)
        self.assertIsInstance(system.operations.data_manager, DataManagerInterface)


if __name__ == '__main__':
    # Run all tests
    unittest.main(verbosity=2)
