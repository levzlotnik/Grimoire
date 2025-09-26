"""
Pydantic types for structured outputs in Golems.

These types can be referenced by name from Prolog configuration
and will be used as output_type for Pydantic AI agents.
"""

from typing import List, Optional, Dict, Any, Union
from pydantic import BaseModel, Field


# Code generation models
class CodeResponse(BaseModel):
    """Response from code generation tasks."""
    code: str = Field(description="Generated code")
    language: str = Field(description="Programming language")
    tests: Optional[List[str]] = Field(default=None, description="Suggested test cases")
    documentation: Optional[str] = Field(default=None, description="Code documentation")
    explanation: Optional[str] = Field(default=None, description="Explanation of the code")


class CodeReview(BaseModel):
    """Code review analysis result."""
    issues: List[Dict[str, str]] = Field(description="List of issues found")
    suggestions: List[str] = Field(description="Improvement suggestions")
    security_concerns: Optional[List[str]] = Field(default=None, description="Security issues")
    performance_notes: Optional[List[str]] = Field(default=None, description="Performance considerations")
    overall_quality: str = Field(description="Overall quality assessment")


# Testing models
class TestResult(BaseModel):
    """Test execution result."""
    passed: int = Field(description="Number of tests passed")
    failed: int = Field(description="Number of tests failed")
    skipped: int = Field(description="Number of tests skipped")
    failures: Optional[List[Dict[str, str]]] = Field(default=None, description="Details of failed tests")
    coverage: Optional[float] = Field(default=None, description="Code coverage percentage")


class TestPlan(BaseModel):
    """Test planning output."""
    test_cases: List[Dict[str, str]] = Field(description="List of test cases to implement")
    coverage_areas: List[str] = Field(description="Code areas to cover")
    edge_cases: List[str] = Field(description="Edge cases to consider")
    test_strategy: str = Field(description="Overall testing strategy")


# Documentation models
class Documentation(BaseModel):
    """Documentation generation result."""
    summary: str = Field(description="Brief summary")
    description: str = Field(description="Detailed description")
    parameters: Optional[List[Dict[str, str]]] = Field(default=None, description="Parameter documentation")
    returns: Optional[str] = Field(default=None, description="Return value documentation")
    examples: Optional[List[str]] = Field(default=None, description="Usage examples")


# Project management models
class ProjectAnalysis(BaseModel):
    """Project structure analysis."""
    structure: Dict[str, Any] = Field(description="Project structure overview")
    dependencies: List[str] = Field(description="Project dependencies")
    entry_points: List[str] = Field(description="Main entry points")
    configuration_files: List[str] = Field(description="Configuration files found")
    recommendations: Optional[List[str]] = Field(default=None, description="Recommendations")


class TaskBreakdown(BaseModel):
    """Task breakdown for project planning."""
    tasks: List[Dict[str, Any]] = Field(description="List of tasks")
    dependencies: Dict[str, List[str]] = Field(description="Task dependencies")
    estimated_effort: Dict[str, str] = Field(description="Effort estimates per task")
    priority_order: List[str] = Field(description="Recommended task order")


# Verification models
class SemanticsVerification(BaseModel):
    """Semantics file verification result."""
    covered_files: List[str] = Field(description="Files covered by tests")
    missing_files: List[str] = Field(description="Files missing test coverage")
    suggestions: Optional[List[str]] = Field(default=None, description="Suggestions for improvement")
    coverage_percentage: float = Field(description="Overall coverage percentage")


class FileAnalysis(BaseModel):
    """File analysis result."""
    file_type: str = Field(description="Type of file")
    purpose: str = Field(description="File purpose")
    key_components: List[str] = Field(description="Key components/functions")
    dependencies: List[str] = Field(description="File dependencies")
    complexity: str = Field(description="Complexity assessment")


# Error and debugging models
class ErrorAnalysis(BaseModel):
    """Error analysis result."""
    error_type: str = Field(description="Type of error")
    description: str = Field(description="Error description")
    likely_causes: List[str] = Field(description="Likely causes")
    suggested_fixes: List[str] = Field(description="Suggested fixes")
    relevant_files: Optional[List[str]] = Field(default=None, description="Relevant files to check")


class DebugPlan(BaseModel):
    """Debugging strategy output."""
    steps: List[str] = Field(description="Debugging steps to follow")
    breakpoints: Optional[List[str]] = Field(default=None, description="Suggested breakpoint locations")
    variables_to_watch: Optional[List[str]] = Field(default=None, description="Variables to monitor")
    hypotheses: List[str] = Field(description="Hypotheses to test")


# Architecture and design models
class ArchitectureReview(BaseModel):
    """Architecture review result."""
    patterns_used: List[str] = Field(description="Design patterns identified")
    strengths: List[str] = Field(description="Architecture strengths")
    weaknesses: List[str] = Field(description="Architecture weaknesses")
    recommendations: List[str] = Field(description="Improvement recommendations")
    diagram: Optional[str] = Field(default=None, description="Architecture diagram in text format")


class RefactoringPlan(BaseModel):
    """Refactoring plan output."""
    targets: List[Dict[str, str]] = Field(description="Refactoring targets")
    strategy: str = Field(description="Overall refactoring strategy")
    steps: List[str] = Field(description="Refactoring steps")
    risk_assessment: str = Field(description="Risk assessment")
    expected_benefits: List[str] = Field(description="Expected benefits")


# Generic models
class Analysis(BaseModel):
    """Generic analysis result."""
    findings: List[str] = Field(description="Key findings")
    data: Dict[str, Any] = Field(description="Analysis data")
    recommendations: Optional[List[str]] = Field(default=None, description="Recommendations")
    confidence: float = Field(description="Confidence level (0-1)")


class Decision(BaseModel):
    """Decision-making result."""
    decision: str = Field(description="The decision made")
    reasoning: str = Field(description="Reasoning behind the decision")
    alternatives: Optional[List[str]] = Field(default=None, description="Alternatives considered")
    confidence: float = Field(description="Confidence level (0-1)")


class Report(BaseModel):
    """Generic report output."""
    title: str = Field(description="Report title")
    sections: List[Dict[str, str]] = Field(description="Report sections")
    summary: str = Field(description="Executive summary")
    conclusions: List[str] = Field(description="Key conclusions")
    next_steps: Optional[List[str]] = Field(default=None, description="Recommended next steps")


# Registry of all available types
TYPE_REGISTRY: Dict[str, type[BaseModel]] = {
    # Code models
    "CodeResponse": CodeResponse,
    "CodeReview": CodeReview,
    
    # Testing models
    "TestResult": TestResult,
    "TestPlan": TestPlan,
    
    # Documentation models
    "Documentation": Documentation,
    
    # Project models
    "ProjectAnalysis": ProjectAnalysis,
    "TaskBreakdown": TaskBreakdown,
    
    # Verification models
    "SemanticsVerification": SemanticsVerification,
    "FileAnalysis": FileAnalysis,
    
    # Error/Debug models
    "ErrorAnalysis": ErrorAnalysis,
    "DebugPlan": DebugPlan,
    
    # Architecture models
    "ArchitectureReview": ArchitectureReview,
    "RefactoringPlan": RefactoringPlan,
    
    # Generic models
    "Analysis": Analysis,
    "Decision": Decision,
    "Report": Report,
}


def get_type_by_name(name: str) -> Optional[type[BaseModel]]:
    """Get a Pydantic type class by its name."""
    return TYPE_REGISTRY.get(name)