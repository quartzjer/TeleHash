/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.eclipse.emf.json.model;

import org.eclipse.emf.ecore.EFactory;

/**
 * <!-- begin-user-doc -->
 * The <b>Factory</b> for the model.
 * It provides a create method for each non-abstract class of the model.
 * <!-- end-user-doc -->
 * @see org.eclipse.emf.json.model.JsonPackage
 * @generated
 */
public interface JsonFactory extends EFactory {
	/**
	 * The singleton instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	JsonFactory eINSTANCE = org.eclipse.emf.json.model.impl.JsonFactoryImpl.init();

	/**
	 * Returns a new object of class '<em>Js Object</em>'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return a new object of class '<em>Js Object</em>'.
	 * @generated
	 */
	JsObject createJsObject();

	/**
	 * Returns the package supported by this factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the package supported by this factory.
	 * @generated
	 */
	JsonPackage getJsonPackage();

} //JsonFactory
