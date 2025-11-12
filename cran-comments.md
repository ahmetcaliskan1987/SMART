## Resubmission

Dear CRAN Team (attn: Konstanze Lauseker),

Thank you very much for your detailed and constructive feedback. We have carefully addressed all the issues mentioned in your previous message.

Below is a summary of the changes made in response to each point:

**1. Single Quotes for Package Names**

We have updated the DESCRIPTION file as requested. All package names in the Title and Description fields (e.g., 'shiny') are now properly enclosed in single quotes.

**2. References for Methods**

The package implements widely established statistical methods (e.g., Cronbach’s Alpha, item-total correlation). As these are standard textbook methods, we did not include external references, but we ensured that the method descriptions are now clear and complete in the DESCRIPTION file.

**3. Usage of `\dontrun{}`, `\donttest{}`, and `if(interactive())`**

We have revised the examples in the documentation accordingly. The example for the main function, which launches a Shiny app, now uses `if(interactive()) {}` instead of `\dontrun{}`. This allows manual testing without interfering with automated CRAN checks.

**4. Tests for Non-Exported Functions (Shiny Interfaces)**

This was the main focus of our revision.  
We refactored the package so that all core logic (data parsing, scoring, and statistical calculations) resides in internal helper functions within the R/ directory.  
A comprehensive **testthat** suite has been added, including **58 unit tests** covering this internal logic.  
All tests pass successfully, ensuring that the package’s computational components can now be automatically checked as requested.

**R CMD check results**

We have run `devtools::check()` locally with the following result:
0 errors | 0 warnings | 0 notes


All checks passed cleanly.

Thank you again for your time and helpful guidance.  
We believe the package now fully meets CRAN submission requirements.

Best regards,  
**Ahmet Çalışkan and Abdullah Faruk Kılıç**
