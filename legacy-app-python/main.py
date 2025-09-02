"""
Main Program - Modernized version of main.cob

This is the main entry point for the Account Management System.
Replaces the COBOL main program with modern Python implementation
while maintaining the same user interface and functionality.
"""

import sys
from typing import Dict, Callable
from interfaces import AccountOperationsInterface, AccountManagementInterface


class AccountManagementSystem(AccountManagementInterface):
    """
    Main application class for the Account Management System.
    
    This class replaces the COBOL MAIN-LOGIC with modern Python
    structure and improved error handling.
    """
    
    def __init__(self, operations: AccountOperationsInterface):
        """
        Initialize the Account Management System with dependency injection.
        
        Args:
            operations (AccountOperationsInterface): Operations dependency (required for DI).
        """
        if operations is None:
            raise ValueError("AccountOperations dependency is required")
        self.operations = operations
        self.continue_flag = True
        
        # Menu options mapping - replaces COBOL EVALUATE statement
        self.menu_options: Dict[str, Callable] = {
            '1': self.operations.view_balance,
            '2': self.operations.credit_account,
            '3': self.operations.debit_account,
            '4': self._exit_program
        }
    
    def display_menu(self) -> None:
        """
        Display the main menu options.
        
        Equivalent to the COBOL DISPLAY statements in MAIN-LOGIC.
        """
        print("\n" + "=" * 40)
        print("Account Management System")
        print("=" * 40)
        print("1. View Balance")
        print("2. Credit Account")
        print("3. Debit Account")
        print("4. Exit")
        print("=" * 40)
    
    def get_user_choice(self) -> str:
        """
        Get user menu choice with input validation.
        
        Replaces COBOL ACCEPT USER-CHOICE with improved validation.
        
        Returns:
            str: User's menu choice
        """
        while True:
            try:
                choice = input("Enter your choice (1-4): ").strip()
                
                if choice in self.menu_options:
                    return choice
                else:
                    print("Invalid choice, please select 1-4.")
                    
            except KeyboardInterrupt:
                print("\nExiting program...")
                return '4'
            except EOFError:
                print("\nInput terminated. Exiting program...")
                return '4'
    
    def execute_choice(self, choice: str) -> None:
        """
        Execute the selected menu option.
        
        Replaces COBOL EVALUATE statement with Python dictionary dispatch.
        
        Args:
            choice (str): User's menu choice
        """
        try:
            action = self.menu_options.get(choice)
            if action:
                action()
            else:
                print("Invalid choice, please select 1-4.")
        except Exception as e:
            print(f"Error executing operation: {e}")
            print("Please try again.")
    
    def _exit_program(self) -> None:
        """
        Handle program exit.
        
        Equivalent to COBOL: MOVE 'NO' TO CONTINUE-FLAG
        """
        self.continue_flag = False
        print("Exiting the program. Goodbye!")
    
    def run(self) -> None:
        """
        Main program loop.
        
        Replaces COBOL PERFORM UNTIL CONTINUE-FLAG = 'NO' loop.
        """
        print("Welcome to the Account Management System")
        
        try:
            while self.continue_flag:
                self.display_menu()
                choice = self.get_user_choice()
                self.execute_choice(choice)
                
        except KeyboardInterrupt:
            print("\n\nProgram interrupted by user. Goodbye!")
        except Exception as e:
            print(f"\nUnexpected error occurred: {e}")
            print("Program will exit.")
        finally:
            if self.continue_flag:  # If not already exited gracefully
                print("Program terminated.")


def main() -> None:
    """
    Entry point for the application.
    
    Equivalent to COBOL STOP RUN.
    Uses dependency injection via factory pattern.
    """
    try:
        # Import here to avoid circular imports
        from factory import create_production_system
        
        app = create_production_system()
        app.run()
        sys.exit(0)
    except Exception as e:
        print(f"Fatal error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
