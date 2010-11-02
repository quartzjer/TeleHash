/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.telehash.model.TelehashPackage
 * @generated
 */
public interface TelehashFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	TelehashFactory eINSTANCE = org.telehash.model.impl.TelehashFactoryImpl
			.init();

	/**
	 * Returns a new object of class '<em>Telex</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Telex</em>'.
	 * @generated
	 */
	Telex createTelex();

	/**
	 * Returns a new object of class '<em>Tap Rule</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Tap Rule</em>'.
	 * @generated
	 */
	TapRule createTapRule();

	/**
	 * Returns a new object of class '<em>Line</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Line</em>'.
	 * @generated
	 */
	Line createLine();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	TelehashPackage getTelehashPackage();

} //TelehashFactory
